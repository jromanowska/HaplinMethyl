#' Creating categorical variables out of continuous data
#'
#' This function prepares the environmental per-SNP data to be used in
#'   stratification when calling \link{haplinStrat}.
#'
#' @param env.data The environmental data, read in by \link{envDataRead}
#'   function.
#' @param breaks Numerical vector indicating how to divide the continuous values
#'   into categories (see Details).
#' @param file.out The core name of the files that will contain the categorized
#'   data (character string); ready to load next time with \link{envDataLoad}
#'   function; default: "env_data_cat".
#' @param dir.out The directory that will contain the saved data; defaults to
#'   current working directory.
#' @param ncpu The number of CPU cores to use - this speeds up the process for
#'   large datasets significantly. Default is 1 core, maximum is 1 less than
#'   the total number of cores available on a current machine (even if the number
#'   given by the user is more than that).
#' @param overwrite Whether to overwrite the output files: if NULL (default),
#'   will prompt the user to give answer; set to TRUE, will automatically
#'   overwrite any existing files; and set to FALSE, will stop if the output
#'   files exist.
#'
#' @return  A list of ff matrices, now containing the categorized data (factors).
#'
#' @section Details:
#'   When 'breaks' is one number, it defines the number of categories that the
#'   range of values will be divided into. This is good only for equally
#'   distributed data. Otherwise, the user is adviced to give 'breaks' as a
#'   vector of break points, similarly to when creating a histogram.
#'
#' @export
#'
envDataCategorize <- function(
	env.data = stop( "You didn't provide the environmental data!",
					 call. = FALSE ),
	breaks,
	file.out = "env_data_cat",
	dir.out = ".",
	ncpu = 1,
	overwrite = NULL
){
	# checking input parameters:
	files.list <- Haplin:::f.make.out.filename( file.out = file.out,
		dir.out = dir.out, overwrite = overwrite )

	if( !inherits( env.data,
				   get( ".class.data.env.cont", envir = .haplinMethEnv ) ) ){
		stop( "Wrong class of the given 'env.data'!", call. = FALSE )
	}

	if( ncpu < 1 ){
		message( "You set 'ncpu' to a number less than 1 - resetting it to 1.\n" )
		ncpu <- 1
	}

	if( length( breaks ) == 1 ){
		if( breaks == 1 ){
			stop( "'Breaks' is 1 - you cannot put all the values in one category.",
				  call. = FALSE )
		}
		if( breaks < 1 ){
			stop( "'Breaks' cannot be less than 1!", call. = FALSE )
		}
	}
	#--------------------------------------------

	find.range.ff <- function( x,y ){
		new.range <- range( c( x, apply( y[,], 1, range ) ) )
		return( new.range )
	}
	if( length( env.data ) > 1 ){
		range.all <- Reduce( f = find.range.ff, x = env.data, init = c() )
	} else {
		range.all <- range( apply( env.data[[ 1 ]][,], 1, range ) )
	}


	if( length( breaks ) == 1 ){
		breaks <- seq( from = range.all[ 1 ],
					   to = range.all[ 2 ],
					   length.out = breaks + 1 )
	} else {
		if( any( breaks > range.all[ 2 ] | breaks < range.all[ 1 ] ) ){
			message( "Some 'breaks' are outside of the range of data values - will remove those." )
			outside.breaks <- which( breaks > range.all[ 2 ] |
									 	breaks < range.all[ 1 ] )
			breaks <- breaks[ -outside.breaks ]
			if( length( breaks ) < 2 ){
				stop( "Too few break points remaining!", call. = FALSE )
			}
		}
	}

	n.bins <- length( breaks ) - 1
	new.levels <- 0:( n.bins - 1 )
	message( paste0( "Creating categories: ", new.levels, collapse = "," ) )

	# -- function that will be called on each element of env.data list
	categorize.per.el <- function( el ){
			env.data.cat <- ff::ff( 0, levels = new.levels,
									dim = dim( el ), vmode = "ushort" )

			out.length <- nrow( el )
			# NOTE: 'el' is an ff matrix, so I can't use apply
			for( col.no in seq_len( ncol( el ) ) ){
				# extract each column as a numeric
				cur.col <- el[, col.no ]
				# factorize
				new.col <- cut( cur.col, breaks = breaks,
								labels = as.character( new.levels ),
								include.lowest = TRUE )
				# and write to the output ff matrix
				env.data.cat[ ,col.no ] <- new.col
			}
			return( env.data.cat )
		}
	# -- alternatively, function that will be called on each column of
	#    the ff matrix with env.data
	categorize.per.col <- function( col.no, el ){
		# extract each column as a numeric
		cur.col <- el[, col.no ]
		# factorize
		new.col <- cut( cur.col, breaks = breaks,
						labels = as.character( new.levels ),
						include.lowest = TRUE )
		# and write to the output ff matrix
		env.data.cat[ ,col.no ] <- new.col
	}

 	if( ncpu == 1 ){
 		message( "Using 1 CPU." )
		# for each element in the list...
		out.data.list <- lapply( env.data, categorize.per.el )
 	} else { # if ncpu > 1
		if( !requireNamespace( "parallel" ) ){
			stop( "You wanted to run a parallel process but the 'parallel' package is not available!" )
		}
		max.ncpu <- parallel::detectCores()
		ncpu <- min( max.ncpu, ncpu )
		message( paste0( "Using ", ncpu, " CPUs." ) )
		cl <- parallel::makeCluster( ncpu, type = "SOCK" )
		invisible( parallel::clusterEvalQ( cl, requireNamespace( "ff",
			quietly = TRUE ) ) )
		invisible( parallel::clusterEvalQ( cl, loadNamespace( "ff" ) ) )
		parallel::clusterExport( cl,
			c( "categorize.per.el", "new.levels", "breaks", "env.data" ),
			envir = environment() )
		#--------
		# this is just to check whether each node on the newly created
		#  "cluster" has access to the data
		open.file.workers <- parallel::clusterEvalQ( cl, length( env.data ) )
		if( !all( unlist( open.file.workers ) == length( env.data ) ) ){
			stop( paste( "Problem with accessing 'env.data' object in workers:",
					which( unlist( open.file.workers ) != length( env.data ) ) ),
				  call. = FALSE )
		}
		#--------

		out.data.list <- parallel::parLapply( cl, env.data, categorize.per.el )
 	}

	class( out.data.list ) <- get( ".class.data.env.cat",
								   envir = .haplinMethEnv )
	return( out.data.list )
}
