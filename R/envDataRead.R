#' Reading the environmental data from a file.
#'
#' This function reads in the environmental data that accompanies the genetic
#'  data read in with \link{genDataRead}.
#'
#' The environmental data such as methylation data can be large if the information
#' is stored on per-SNP basis. Thus, when data is large, this function reads it
#' in andcreates a special ff object that stores the data without limiting the
#' memory available. This can take time but needs to be performed only once.
#' Later on, one can use the \link{envDataLoad} function to load the appropriate
#' data from \code{.ffData} file saved to disk, which is a quick process.
#'
#' @param file.in The name of the file with environmental data.
#' @param dir.in The path to the directory where the 'file.in' resides.
#' @param file.out The base name for the output files (see Details).
#' @param dir.out The path to the directory where the output files will be saved.
#' @param sep The separator character that separates values in each line of the
#'   file; "," by default (as in a csv file).
#' @param cont Logical - are the values continuous (TRUE, default) or categories
#'   (FALSE)? See Details.
#' @param header Logical indicating whether the first line of the file is a header;
#'   default TRUE.
#' @param rownames Default (TRUE) indicates that the first column of the file
#'   includes names of rows. If a character vector is given here, these names
#'   are used as rownames; if FALSE, no rownames are used.
#' @param overwrite Logical: if a file with the given name exists, should it be
#'   overwritten or not? If NULL, the user will be prompt for input.
#'
#' @return A list of ffdf objects with the environmental data in numeric format.
#'
#' @section Details:
#' If 'file.out' is not given, the default is NULL and the output filenames are
#'   constructed based on the input filenames. The '_env' suffix is added to the
#'   base name and the \code{.ffData} file is written to disk. This file contains
#'   all the information needed to restore the ffdf object by calling
#'   \link{envDataLoad} function later on.
#'
#'   If 'cont' is TRUE (default), the output data will be a list of ff matrices
#'   containing single-precision values. However, before using this data as
#'   stratification values, the user needs to create categories - this can be
#'   done manually or with the provided \link{envDataCategorize} function.
#'
#' @export
#'
envDataRead <- function( file.in = stop( "'file.in' must be given!" ),
	dir.in = ".",
	file.out = NULL,
	dir.out = ".",
	sep = ",",
	cont = TRUE,
	header = TRUE,
	rownames = TRUE,
	overwrite = NULL ){

	full.path <- file.path( dir.in, file.in )

	if( !file.exists( full.path ) ){
		stop( "The file(s) doesn't seem to exist!", call. = FALSE )
	}

	files.list <- Haplin:::f.make.out.filename(
		file.in, file.out, dir.out = dir.out,
		root = "env", overwrite = overwrite )

	rownames.first.col <- FALSE
	env.rownames <- c()
	if( is.logical( rownames ) ){
		rownames.first.col <- rownames
	} else {
		env.rownames <- rownames
	}

	## read the first line of the file and check the number of columns
	first.line <- scan( file = full.path, what = "character", nlines = 1,
						strip.white = TRUE, sep = sep )
	nb.cols <- length( first.line )

	## the chunk size that would still fit in the memory
	nb.lines.per.chunk <- ceiling( 100000000 / nb.cols )

	## open the file
	in.file <- file( full.path, "r" )

	env.data.in.ff <- list()
	env.levels <- c()

	nb.rows.tot <- 0
	i <- 1
	message( "Reading the data in chunks...\n" )
	message( " -- chunk ", i, "-- \n" )
	cur.chunk <- suppressWarnings(
		matrix(
			scan( in.file, what = "character",
				  nlines = nb.lines.per.chunk,
				  sep = sep ),
			ncol = nb.cols, byrow = TRUE
			)
		)
	if( ncol( cur.chunk ) == 1 &
		rownames.first.col ){
		stop( "Problem with data: have you provided a correct separator?",
			  call. = FALSE )
	} else {
		if( header ){
			env.colnames <- as.character( cur.chunk[ 1, ] )
			cur.chunk <- cur.chunk[ -1, ]
			if( rownames.first.col ){
				env.colnames <- env.colnames[ -1 ]
			}
		}
	}

	## reading in chunks and creating ff object for each chunk
	while( length( cur.chunk ) != 0 ){
		if( rownames.first.col ){
			env.rownames <- c( env.rownames, as.character( cur.chunk[ ,1 ] ) )
			cur.chunk <- cur.chunk[ ,-1 ]
		}
		if( !cont ){ # data can be put in several categories
			cur.levels <- unique( as.vector( cur.chunk ) )
			env.levels <- as.character( union( env.levels, cur.levels ) )

			tmp.ff <- ff::as.ff( cur.chunk,
								 vmode = .haplinMethEnv$.vmode.gen.data,
								 levels = env.levels )
		} else { # data consists of continuous variables
			tmp.ff <- ff::as.ff( cur.chunk, vmode = "single" )
		}

		env.data.in.ff <- c( env.data.in.ff, list( tmp.ff ) )
		nb.rows.tot <- nb.rows.tot + nrow( tmp.ff )

		rm( cur.chunk, tmp.ff )

		i <- i + 1
		message( " -- chunk ", i, "-- \n" )
		cur.chunk <- matrix(
			scan( in.file, what = "character",
				  nlines = nb.lines.per.chunk,
				  sep = sep ),
			ncol = nb.cols, byrow = TRUE
			)
	}
	message( "... done reading.\n\n" )

	close( in.file )

	## re-organize - it's much better to have a list with different column-chunks
	nb.cols.per.chunk <- get( ".nb.cols.per.chunk",
							  envir = .haplinMethEnv )
	nb.cols.env.data <- ncol( env.data.in.ff[[1]] )
	nb.col.chunks <- ceiling( nb.cols.env.data / nb.cols.per.chunk )
	env.data.col.wise <- list()

	message( "Preparing data...\n" )
	for( i in 1:nb.col.chunks ){
		cur.cols <- ( ( i-1 )*nb.cols.per.chunk + 1 ):
					  ( min( i*nb.cols.per.chunk, nb.cols.env.data ) )
		if( !cont ){
			tmp.env.data <- ff::ff( vmode = .haplinMethEnv$.vmode.gen.data,
				levels = env.levels,
				dim = c( nb.rows.tot,
						 min( nb.cols.per.chunk,
						 	 max( cur.cols ) - min( cur.cols ) + 1
						 	 )
						 )
				)
		} else {
			tmp.env.data <- ff::ff( vmode = "single",
				dim = c( nb.rows.tot,
					min( nb.cols.per.chunk,
						 max( cur.cols ) - min( cur.cols ) + 1
						 )
					)
				)
		}
		prev.rows <- 0
		for( j in seq_along( env.data.in.ff ) ){
			cur.rows <- ( prev.rows + 1 ):( prev.rows + nrow( env.data.in.ff[[j]] ) )
			tmp.env.data[ cur.rows, ] <- env.data.in.ff[[j]][ ,cur.cols ]
			prev.rows <- max( cur.rows )
		}
		if( header ){
			colnames( tmp.env.data ) <- env.colnames[ cur.cols ]
		}
		if( !is.null( env.rownames ) ){
			rownames( tmp.env.data ) <- env.rownames
		}
		env.data.col.wise <- c( env.data.col.wise, list( tmp.env.data ) )
		rm( tmp.env.data )
	}
	message( "... done preparing \n\n" )

	rm( env.data.in.ff )

	## saving the data in the .RData and .ffData files
	message( "Saving data... \n" )
	save.list <- c()
	for( i in seq_along( env.data.col.wise ) ){
		cur.name <- paste( get( ".env.cols.name", envir = .haplinMethEnv ), i,
						   sep = "." )
		assign( cur.name, env.data.col.wise[[i]] )
		save.list <- c( save.list, cur.name )
	}
	save.list <- c( save.list, "cont" )
	ff::ffsave( list = save.list,
				file = file.path( dir.out, files.list$file.out.base ) )
	message( "... saved to file: ", files.list$file.out.ff, "\n" )

	if( cont ){
		class( env.data.col.wise ) <- get( ".class.data.env.cont",
										   envir = .haplinMethEnv )
	} else {
		class( env.data.col.wise ) <- get( ".class.data.env.cat",
										   envir = .haplinMethEnv )
	}
	return( env.data.col.wise )
}
