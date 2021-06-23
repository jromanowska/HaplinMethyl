#' Creating categorical variables out of continuous data
#'
#' This function prepares the environmental data to be used in
#'   stratification when calling \link{haplinStrat}.
#'
#' @param env.data The environmental data, read in by \link{envDataRead}
#'   function.
#' @param summary.method If there are more than one probe (rows), this method is
#'   used to summarize the continuous data across columns to create one number
#'   per row (sample), which will be then used to categorize data.
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
#'   The function also creates two files: .RData and .ffData.
#'
#' @section Details:
#'   The `env.data` given here is assumed to be a set that is somehow linked to
#'   each other, e.g., if the data is DNA methylation measurements for various
#'   CpGs, the CpGs might be from one region around a given SNP.
#'   
#'   The `summary.method` argument takes a value from the following list:
#'   \itemize{
#'     \item `sum` - arithmetical sum of the values (default);
#'     \item `average` - average value;
#'     \item `NULL` - no summary; **NOTE:** if the data contains more than one
#'       column, be sure to check that the breaks give correct division for
#'       each of the column!
#'   }
#'   
#'   When `breaks` is one number, it defines the number of categories that the
#'   range of values will be divided into. The categories will be equal in size,
#'   based on the appropriate quantiles calculated from the summarized values.
#'
#' @export
#'
envDataCategorize <- function(
	env.data = stop( "You didn't provide the environmental data!",
					 call. = FALSE ),
	summary.method = "sum",
	breaks,
	file.out = "env_data_cat",
	dir.out = ".",
	overwrite = NULL
){
	# checking input parameters:
	files.list <- Haplin:::f.make.out.filename( file.out = file.out,
		dir.out = dir.out, overwrite = overwrite )

	if( !inherits( env.data,
				   get( ".class.data.env.cont", envir = .haplinMethEnv ) ) ){
		stop( "Wrong class of the given 'env.data'!", call. = FALSE )
	}

	if( length( breaks ) == 1 ){
		if( breaks == 1 ){
			stop( "'Breaks' is 1 - you cannot put all the values in one category.",
				  call. = FALSE )
		}
		if( breaks < 1 ){
			stop( "'Breaks' cannot be less than 1!", call. = FALSE )
		}
	} else if( length( breaks ) < 1 ){
			stop( "Too few break points!", call. = FALSE )
	}
	#--------------------------------------------

	# --- summarize the values ----
  # extract the measurements as one big ff matrix
  all_env_data_ff <- Haplin:::f.get.gen.data.cols(
    gen.data = env.data,
    cols = 1:ncolumns(env.data)
  )
  orig_rownames <- summary(env.data, short = FALSE)$rownames
  
	if(!is.null(summary.method)){
	  out_env_data <- switch (summary.method,
	    sum = {
	      ff::ffrowapply(
	        EXPR = rowSums(all_env_data_ff[,,drop = FALSE]),
	        X = all_env_data_ff,
	        RETURN = TRUE,
	        CFUN = "c"
	      )
	    },
	    average = {
	      ff::ffrowapply(
	        EXPR = rowSums(all_env_data_ff[,,drop = FALSE])/ncol(all_env_data_ff),
	        X = all_env_data_ff,
	        RETURN = TRUE,
	        CFUN = "c"
	      )
	    }
	  )
	  out_env_data_ff <- ff::ff(
	    as.numeric(out_env_data),
	    dim = c(length(out_env_data), 1)
	   )
	} else {
	  # don't summarize
	  out_env_data_ff <- all_env_data_ff
	}
  rownames(out_env_data_ff) <- orig_rownames
	
  # --- check break points ----
	if( length( breaks ) == 1 ){
	  new_breaks <- as.numeric(ff::ffcolapply(
	    EXPR = quantile(
	      out_env_data_ff[,,drop=FALSE],
	      probs = seq.int(from = 0, to = breaks)/breaks
	    ),
	    X = out_env_data_ff,
	    RETURN = TRUE,
	    CFUN = "c"
	   ))
	} else {
	  new_breaks <- breaks
	}

  n_bins <- length( new_breaks ) - 1
	new_levels <- 1:n_bins
	message( "Creating categories: ", paste( new_levels, collapse = "," ) )

  # --- categorize ----
  cat_env_data <- ff::ff(
		initdata = ff::ffcolapply(
      EXPR = cut(
        out_env_data_ff[,, drop = FALSE],
        breaks = new_breaks,
        include.lowest = TRUE,
        labels = FALSE
      ),
      X = out_env_data_ff,
      RETURN = TRUE,
      CFUN = "c",
      USE.NAMES = FALSE
  	),
		levels = new_levels,
		dim = c(nrows(env.data), ncol(out_env_data_ff)),
		vmode = "byte"
  )
	rownames(cat_env_data) <- orig_rownames
	colnames(cat_env_data) <- colnames(out_env_data_ff)
	
	## saving the data in the .RData and .ffData files
	message( "Saving data... \n" )
	# this is not a continuous data anymore
	cont <- FALSE
	cur.name <- paste( get( ".env.cols.name", envir = .haplinMethEnv ), "1",
					   sep = "." )
	assign( cur.name, cat_env_data )
	save.list <- c( cur.name, "cont" )
	ff::ffsave( list = save.list,
				file = file.path( dir.out, files.list$file.out.base ) )
	message( "... saved to file: ", files.list$file.out.ff, "\n" )

	out_list <- list(cat_env_data)
	class(out_list) <- get( ".class.data.env.cat",
									   envir = .haplinMethEnv )
	return(out_list)
}
