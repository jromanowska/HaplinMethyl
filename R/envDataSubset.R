#' Subsetting the environmental data
#'
#' Extract a subset of environmental data by specifying rows and/or columns.
#'
#' @param env.data The dataset (class \code{env.data}) from which to extract data.
#' @param col.names Character vector with column names to extract (optional).
#' @param row.names Character vector with row names to extract (optional).
#' @param col.ids Numeric vector with column IDs (optional).
#' @param row.ids Numeric vector with row IDs (optional).
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
						   row.ids
){
	# checking input args
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

		all.col.names <- info.env.data$col.names
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

			final.col.sel <- match( col.names, all.col.names )
		}
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

		all.row.names <- info.env.data$row.names
		if( is.null( all.row.names ) ){
			stop( "The input data does not contain column names. Use the 'row.ids'
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

			final.row.sel <- match( row.names, all.row.names )
		}
	}

	# ---- col.ids ----
	if( !missing( col.ids ) ){
		if( !is.integer( col.ids ) ){
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
		if( !is.integer( row.ids ) ){
			stop( "Parameter 'row.ids' should be an integer vector!", call. =  FALSE )
		}
		if( any( row.ids < 1 ) |
			any( row.ids > info.env.data$nrow ) ){
			stop( "You chose row numbers that don't exist!", call. = FALSE )
		}

		final.row.sel <- row.ids
	}

	# ---- subsetting ----
	if( is.logical( final.col.sel ) ){
		# no col subsetting
		final.col.sel <- 1:info.env.data$ncol
	} else {
		message( paste( "Will select", length( final.col.sel ), "columns." ) )
	}
	if( is.logical( final.row.sel ) ){
		# no row subsetting
		final.row.sel <- 1:info.env.data$nrow
	} else {
		message( paste( "Will select", length( final.row.sel ), "rows." ) )
	}

	# # TODO: change here, the following is just copied from Haplin::genDataGetPart
	# ncols.per.chunk <- ncol( env.data[[ 1 ]] )
	# if( length( final.col.sel ) > ncols.per.chunk ){
	# 	warning( "You're requesting big amount of data!" )
	# }
	# col.info <- Haplin:::f.get.which.gen.el( final.col.sel, ncols.per.chunk )
	# which.gen.chunk <- col.info$chunk.no
	# which.cols.chunk <- col.info$col.no
	#
	# new.dim <- c( length( final.row.sel ), length( final.col.sel ) )
	# new.colnames <- c()
	# new.rownames <- c()
	# gen.data.out <- ff::ff( NA, levels = all.levels, dim = new.dim, vmode = ff::vmode( gen.data[[ 1 ]] ) )
	# for( i in 1:length( cols ) ){
	# 	gen.data.out[ ,i ] <- gen.data[[ which.gen.chunk[ i ] ]][ ,which.cols.chunk[ i ] ]
	# 	new.colnames <- c( new.colnames,
	# 		colnames( gen.data[[ which.gen.chunk[ i ] ]] )[ which.cols.chunk[ i ] ] )
	# }
	# colnames( gen.data.out ) <- new.colnames
	#
	#
	# if( is.subset.cols ){
	# 	# check how many chunks will be needed
	# 	nb.cols.per.chunk <- get( ".nb.cols.per.chunk", envir = .haplinEnv )
	# 	nb.chunks <- ceiling( length( subset.cols ) / nb.cols.per.chunk )
	#
	# 	gen.data.col.wise <- sapply( 1:nb.chunks, function( x ){
	# 		first.col <- ( nb.cols.per.chunk*( x - 1 ) + 1 )
	# 		last.col <- min( ( nb.cols.per.chunk*x ), length( subset.cols ) )
	# 		cur.cols <- subset.cols[ first.col:last.col ]
	# 		list( f.get.gen.data.cols( data.in$gen.data, cur.cols ) )
	# 	} )
	# } else {
	# 	gen.data.col.wise <- data.in$gen.data
	# }
	#
	# cov.data.in <- NULL
	# if( is.subset.rows ){
	# 	# need to choose from both gen.data and cov.data
	# 	gen.data.col.wise <- lapply( gen.data.col.wise, function( x ){
	# 		sub <- x[ subset.rows, ]
	# 		out <- ff::ff( sub, levels = ff::levels.ff( sub ), dim = dim( sub ), vmode = ff::vmode( x ) )
	# 		colnames( out ) <- colnames( sub )
	# 		return( out )
	# 	})
	#
	# 	if( !is.null( data.in$cov.data ) ){
	# 		cov.data.in <- data.in$cov.data[ subset.rows, ]
	# 	}
	# } else if( !is.null( data.in$cov.data ) ){
	# 	cov.data.in <- data.in$cov.data
	# }
	# data.out <- list( cov.data = cov.data.in, gen.data = gen.data.col.wise, aux = data.in$aux )
	# class( data.out ) <- class( data.in )
	#
	# ## saving the chosen part of the data
	# cat( "Saving data... \n" )
	# cur.names <- c()
	# for( i in 1:length( gen.data.col.wise ) ){
	# 	cur.name <- paste( get( ".gen.cols.name", envir = .haplinEnv ), i, sep = "." )
	# 	assign( cur.name, gen.data.col.wise[[i]] )
	# 	cur.names <- c( cur.names, cur.name )
	# }
	# aux <- data.in$aux
	# save.list <- c( cur.names, "aux" )
	# if( !is.null( cov.data.in ) ){
	# 	save.list <- c( save.list, "cov.data.in" )
	# }
	# ff::ffsave( list = save.list, file = file.path( dir.out, files.list$file.out.base ) )
	# cat( "... saved to files: ", files.list$file.out.ff, ", ", files.list$file.out.aux, "\n", sep = "" )
	#
	# return( data.out )
}
