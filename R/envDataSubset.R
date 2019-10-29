#' Subsetting the environmental data
#'
#' Extract a subset of environmental data by specifying rows and/or columns.
#'
#' @param env.data The dataset (class \code{env.data}) from which to extract data.
#' @param col.names Character vector with column names to extract (optional).
#' @param row.names Character vector with row names to extract (optional).
#' @param col.ids Numeric vector with column IDs (optional).
#' @param row.ids Numeric vector with row IDs (optional).
#' @param file.out The base for the output filename (default: "my_data_part").
#' @param dir.out The path to the directory where the output files will be saved.
#' @param overwrite Whether to overwrite the output files: if NULL (default), will prompt
#'   the user to give answer; set to TRUE, will automatically overwrite any existing files;
#'   and set to FALSE, will stop if the output files exist.
#'
#' @section Warning:
#'   The parameter \code{col.names} cannot be used jointly with \code{col.ids}, similarly
#'   for the rows!
#'
#' @return A matrix with the subset of data.
#'
#' @export
envDataSubset <- function( env.data = stop( "You need to specify the data!", call. = FALSE ),
						   col.names,
						   row.names,
						   col.ids,
						   row.ids,
						   file.out = "my_data_part",
						   dir.out = ".",
						   overwrite = NULL
){
	# checking input args
	files.list <- Haplin:::f.make.out.filename( file.out = file.out,
		dir.out = dir.out, overwrite = overwrite )

	if( nargs() == 1 ){
		stop( "Have you forgotten to specify what do you want?", call. = FALSE )
	}
	if( !( "env.data" %in% class( env.data ) ) ){
		stop( "Wrong class of the input data set!", call. = FALSE )
	}

	info.env.data <- summary( env.data )
	final.col.sel <- TRUE
	final.row.sel <- TRUE

	#---- col.names ----
	if( !missing( col.names ) ){
		if( !missing( col.ids ) ){
			stop( "Parameter 'col.names' cannot be used jointly with 'col.ids'.
				  Please choose one.", call. = FALSE )
		}
		if( !is.character( col.names ) ){
			stop( "Parameter 'col.names' should be a character vector!",
				  call. = FALSE )
		}

		all.col.names <- info.env.data$colnames
		if( is.null( all.col.names ) ){
			stop( "The input data does not contain column names. Use the 'col.ids'
				  parameter instead and try again.", call. = FALSE )
		}
		if( !all( col.names %in% all.col.names ) ){
			which.wrong.col.names <- which( ! ( col.names %in% all.col.names ) )
			col.names <- col.names[ -which.wrong.col.names ]
			if( length( col.names ) == 0 ){
				stop( "The given 'col.names' do not include any name within the column
					  names of the 'env.data'! Check and try once more.",
					  call. = FALSE )
			}

			warning( paste(
					"The given 'col.names' include some names not found within the
					 column names of 'env.data'. These have been removed.
					 Proceeding with the remaining selection: \n", col.names,
					collapse = "," ) )

		}
		final.col.sel <- match( col.names, all.col.names )
	}

	# ---- row.names ----
	if( !missing( row.names ) ){
		if( !missing( row.ids ) ){
			stop( "Parameter 'row.names' cannot be used jointly with 'row.ids'.
				  Please choose one.", call. = FALSE )
		}
		if( !is.character( row.names ) ){
			stop( "Parameter 'row.names' should be a character vector!",
				  call. = FALSE )
		}

		all.row.names <- info.env.data$rownames
		if( is.null( all.row.names ) ){
			stop( "The input data does not contain row names. Use the 'row.ids'
				  parameter instead and try again.", call. = FALSE )
		}
		if( !all( row.names %in% all.row.names ) ){
			which.wrong.row.names <- which( ! ( row.names %in% all.row.names ) )
			row.names <- row.names[ -which.wrong.row.names ]
			if( length( row.names ) == 0 ){
				stop( "The given 'row.names' do not include any name within the row
					  names of the 'env.data'! Check and try once more.",
					  call. = FALSE )
			}

			warning( paste(
					"The given 'row.names' include some names not found within the
					 row names of 'env.data'. These have been removed.
					 Proceeding with the remaining selection: \n", row.names,
					collapse = "," ) )

		}
		final.row.sel <- match( row.names, all.row.names )
	}

	# ---- col.ids ----
	if( !missing( col.ids ) ){
		if( !is.numeric( col.ids ) ){
			stop( "Parameter 'col.ids' should be an integer vector!", call. =  FALSE )
		}
		if( any( col.ids < 1 ) |
			any( col.ids > info.env.data$ncol ) ){
			stop( "You chose column numbers that don't exist!", call. = FALSE )
		}

		final.col.sel <- col.ids
	}

	# ---- row.ids ----
	if( !missing( row.ids ) ){
		if( !is.numeric( row.ids ) ){
			stop( "Parameter 'row.ids' should be an integer vector!", call. =  FALSE )
		}
		if( any( row.ids < 1 ) |
			any( row.ids > info.env.data$nrow ) ){
			stop( "You chose row numbers that don't exist!", call. = FALSE )
		}

		final.row.sel <- row.ids
	}

	# ---- subsetting ----
	is.subset.cols <- TRUE
	if( is.logical( final.col.sel ) ){
		# no col subsetting
		is.subset.cols <- FALSE
		final.col.sel <- 1:info.env.data$ncol
	} else {
		message( paste( "Will select", length( final.col.sel ), "columns." ) )
	}

	is.subset.rows <- TRUE
	if( is.logical( final.row.sel ) ){
		# no row subsetting
		is.subset.rows <- FALSE
		final.row.sel <- 1:info.env.data$nrow
	} else {
		message( paste( "Will select", length( final.row.sel ), "rows." ) )
	}

	# get the numbers of columns and chunks (env.data is a list structure!)
	ncols.per.chunk <- ncol( env.data[[ 1 ]] )
	col.info <- Haplin:::f.get.which.gen.el( final.col.sel, ncols.per.chunk )
	which.gen.chunk <- col.info$chunk.no
	which.cols.chunk <- col.info$col.no

	new.dim <- c( length( final.row.sel ), length( final.col.sel ) )
	new.colnames <- c()
	new.rownames <- c()

	# create new ff object, with the new dimensions
	cont.data <- "env.cont" %in% info.env.data$class
	if( !cont.data ){
		data.out <- ff::ff( NA,
							levels = levels( env.data[[ 1 ]] ),
							dim = new.dim,
							vmode = ff::vmode( env.data[[ 1 ]] ) )
	} else {
		data.out <- ff::ff( NA,
							dim = new.dim,
							vmode = ff::vmode( env.data[[ 1 ]] ) )
	}

	if( is.subset.cols ){
		# check how many chunks will be needed
		nb.cols.per.chunk <- get( ".nb.cols.per.chunk", envir = Haplin:::.haplinEnv )
		nb.chunks <- ceiling( length( final.col.sel ) / nb.cols.per.chunk )

		# extract data, chunk by chunk
		env.data.col.wise <- sapply( 1:nb.chunks, function( x ){
			first.col <- ( nb.cols.per.chunk*( x - 1 ) + 1 )
			last.col <- min( ( nb.cols.per.chunk*x ), length( final.col.sel ) )
			cur.cols <- final.col.sel[ first.col:last.col ]
			out <- Haplin:::f.get.gen.data.cols( env.data, cur.cols )
			rownames( out ) <- rownames( env.data[[ 1 ]] )
			return( list( out ) )
		} )
	} else {
		# no column subsetting
		env.data.col.wise <- env.data
	}

	# if subsetting rows
	if( is.subset.rows ){
		data.out <- lapply( env.data.col.wise, function( x ){
			sub <- x[ final.row.sel, ]
			if( !cont.data ){
				out <- ff::ff( sub, levels = ff::levels.ff( sub ),
							   dim = dim( sub ),
							   vmode = ff::vmode( x ) )
			} else {
				out <- ff::ff( sub,
							   dim = dim( sub ),
							   vmode = ff::vmode( x ) )
			}
			colnames( out ) <- colnames( sub )
			rownames( out ) <- rownames( x )[ final.row.sel ]
			return( out )
		})
	} else {
		data.out <- env.data.col.wise
	}

	class( data.out ) <- class( env.data )

	# ---- saving the chosen part of the data----
	cat( "Saving data... \n" )
	cur.names <- c()
	for( i in 1:length( env.data.col.wise ) ){
		cur.name <- paste( get( ".env.cols.name", envir = .haplinMethEnv ), i, sep = "." )
		assign( cur.name, env.data.col.wise[[i]] )
		cur.names <- c( cur.names, cur.name )
	}
	save.list <- c( cur.names, "cont.data" )
	ff::ffsave( list = save.list, file = file.path( dir.out, files.list$file.out.base ) )
	cat( "... saved to files: ", files.list$file.out.ff, ", ",
		 files.list$file.out.aux, "\n", sep = "" )

	return( data.out )
}
