#' Reading the environmental data from an object.
#'
#' This function reads in environmental data, such as DNA methylation data in
#' the form of beta values, from a matrix in memory.
#'
#' The environmental data such as methylation data can be large if the information
#' is stored on per-SNP basis. Thus, when data is large, this function reads
#' it in and creates a special ff object that stores the data without limiting
#' the memory available. This can take time but needs to be performed only once.
#' Later on, one can use the \link{envDataLoad} function to load the appropriate
#' data from \code{.ffData} file saved to disk, which is a quick process.
#'
#' @param obj.in The object (matrix) with the environmental data.
#' @param file.out The base name for the output files (see Details).
#' @param dir.out The path to the directory where the output files will be
#'   saved.
#' @param cont Logical - are the values continuous (TRUE, default) or categories
#'   (FALSE)? See Details.
#' @param overwrite Logical: if a file with the given name exists, should it be
#'   overwritten or not? If NULL, the user will be prompt for input.
#'
#' @return A list of \code{ff_matrix} objects with the environmental data in
#'   numeric format.
#'
#' @section Details: If 'file.out' is not given, the default is NULL and the
#'   output filenames are constructed based on the input filenames. The suffix
#'   '_env' is added to the base name and the \code{.ffData} file is written to
#'   disk. This file contains all the information needed to restore the list
#'   with \code{ff_matrix} objects by calling \link{envDataLoad} function later
#'   on.
#'
#'   If 'cont' is TRUE (default), the output data will be a list of ff matrices
#'   containing single-precision values. However, before using this data as
#'   stratification values, the user needs to create categories - this can be
#'   done manually or with the provided \link{envDataCategorize} function.
#'
#' @export
#'
envDataReadFromObj <- function(
	obj.in = stop( "The object must be given!", .call = FALSE ),
	file.out = NULL,
	dir.out = ".",
	cont = TRUE,
	overwrite = NULL
){

	file.in <- "my_data" # just to create the 'file.out'
	files.list <- Haplin:::f.make.out.filename( file.in,
		file.out, dir.out = dir.out, root = "env", overwrite = overwrite )

	if( !is.matrix( obj.in ) ){
		stop( "The given object is not a matrix", .call = FALSE )
	}

	## a list with different column-chunks
	nb.cols.per.chunk <- get( ".nb.cols.per.chunk", envir = .haplinMethEnv )
	nb.cols.env.data <- ncol( obj.in )
	nb.col.chunks <- ceiling( nb.cols.env.data / nb.cols.per.chunk )
	nb.rows.tot <- nrow( obj.in )
	env.colnames <- colnames( obj.in )
	env.rownames <- rownames( obj.in )
	env.data.col.wise <- list()
	if( !cont ){
		env.levels <- unique( obj.in )
	}

	message( "Preparing data...\n" )
	for( i in 1:nb.col.chunks ){
		cur.cols <- ( ( i-1 )*nb.cols.per.chunk + 1 ):
					  ( min( i*nb.cols.per.chunk, nb.cols.env.data ) )
		if( !cont ){
			tmp.env.data <- ff::ff( obj.in[ ,cur.cols ],
				vmode = .haplinMethEnv$.vmode.gen.data,
				levels = env.levels,
				dim = c(
					nb.rows.tot,
					min( nb.cols.per.chunk, max( cur.cols ) - min( cur.cols ) + 1 )
					)
				)
		} else {
			tmp.env.data <- ff::ff( obj.in[ ,cur.cols ],
				vmode = "single",
				dim = c(
					nb.rows.tot,
					min( nb.cols.per.chunk, max( cur.cols ) - min( cur.cols ) + 1 )
					)
				)
		}
		colnames( tmp.env.data ) <- env.colnames[ cur.cols ]
		rownames( tmp.env.data ) <- env.rownames
		env.data.col.wise <- c( env.data.col.wise, list( tmp.env.data ) )
		rm( tmp.env.data )
	}
	message( "... done preparing \n\n" )

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
