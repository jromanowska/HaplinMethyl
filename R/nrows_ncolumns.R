#' Number of rows/columns of env.data
#'
#' \code{nrows} and \code{ncolumns} return the number of rows or columns present in obj.
#'
#' @param obj env.cont or env.cat data, read in by \link{envDataRead}.
#'
#' @return integer
#'
#' @export
nrows <- function( obj ){
	if( !( "env.data" %in% class( obj ) ) ){
		stop( paste( "Unknown object of class", class( obj ), collapse = "," ),
			  call. = FALSE )
	}

	return( nrow( obj[[ 1 ]] ) )
}

#' @rdname nrows
#' @export
ncolumns <- function( obj ){
	if( !( "env.data" %in% class( obj ) ) ){
		stop( paste( "Unknown object of class", class( obj ), collapse = "," ),
			  call. = FALSE )
	}

	ncol.all <- unlist( lapply( obj, ncol ) )
	return( sum( ncol.all ) )
}
