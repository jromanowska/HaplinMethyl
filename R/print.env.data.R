#' @keywords internal
print.env.data <- function( x, cont = TRUE ){
	if( cont ){
		what.data <- "continuous"
	} else {
		what.data <- "categorical"
	}

	cat( paste( "This is", what.data, "environmental data read in by 'envDataRead'\n" ) )
	cat( paste( "with", ncolumns( x ), "columns\n" ) )
	cat( paste( "and", nrows( x ), "rows." ) )
}

#' Info about the \code{env.cat} class
#'
#' Prints a short summary about the categorical environmental data.
#'
#' @param x The \code{env.cat} object read in by \link{envDataRead}.
#'
#' @return \code{NULL}
#'
#' @export
print.env.cat <- function( x, ... ){
	print.env.data( x, cont = FALSE )
}

#' Info about the \code{env.cont} class
#'
#' Prints a short summary about the continuous environmental data.
#'
#' @param x The \code{env.cont} object read in by \link{envDataRead}.
#'
#' @return \code{NULL}
#'
#' @export
print.env.cont <- function( x, ... ){
	print.env.data( x, cont = TRUE )
}
