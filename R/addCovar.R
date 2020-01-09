#' Adding a covariate variable
#'
#' Easily add a column to \code{cov.data} part of the "haplin.data" or "haplin.ready" object
#'
#' Add a character or integer vector to the "cov.data" part of the "haplin.data" or
#'   "haplin.ready" object. This is useful e.g., when preparing for stratification
#'   analysis (with \code{\link{haplinStrat}}). Note that all the parameters are required
#'   and that \code{covar} needs to be a vector of the length equal to the number of
#'   rows in the \code{data.in}.
#'
#' @param data.in The input data, "haplin.data" or "haplin.ready" object.
#' @param covar The vector to be added.
#' @param c.name The name of the column that will be added to \code{cov.data}.
#'
#' @return An object of the same class as \code{data.in} but with additional covariates.
#'
#' @export
#'
addCovar <- function(
	data.in = stop( "Data.in must be given!", call. = FALSE )
	, covar = stop( "covar must be given!", call. = FALSE )
	, c.name = stop( "c.name must be given!", call. = FALSE )
){
	# check the input params
	if( !( inherits( data.in, "haplin.data" ) || inherits( data.in, "haplin.ready" ) ) ){
		stop( "The input data is not in the correct format!", call. = FALSE )
	}

	if( !is.vector( covar ) ){
		stop( "'covar' is not a vector!", call. = FALSE )
	}
	if( length( covar ) != nrow( data.in$gen.data[[ 1 ]] ) ){
		stop( paste0( "'covar' length (", length( covar ), ") is not equal to
					  the number of rows in the data (", nrow( data.in$gen.data[[ 1 ]] ),
					  ")!"), call. = FALSE )
	}

	if( length( c.name ) != 1 ){
		stop( "'c.name' should be just one string!", call. = FALSE )
	}

	# adding the data
	new.data <- data.in
	if( is.null( data.in$cov.data ) ){
		# need to create new cov.data
		new.data$cov.data <- matrix( data = covar, ncol = 1 )
		colnames( new.data$cov.data ) <- c.name
	} else {
		new.data$cov.data <- cbind( data.in$cov.data, covar )
		colnames( new.data$cov.data ) <- c( colnames( data.in$cov.data ), c.name )
	}

	return( new.data )
}
