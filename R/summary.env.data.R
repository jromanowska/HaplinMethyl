#' @keywords internal
summary.env.data <- function( object, short, cont = TRUE ){
	all.rownames <- rownames( object[[ 1 ]] )
	all.colnames.list <- lapply( object, colnames )
	all.colnames <- do.call( what = c, args = all.colnames.list )

	out <- list( class = class( object ),
				  nrow = nrows( object ),
				  ncol = ncolumns( object ),
				  rownames = all.rownames,
				  colnames = all.colnames
				  )
	if(short){
	  return(str(out))
	}
	return(out)
}

#' Info about the \code{env.cont} class
#'
#' Returns a short summary about the continuous environmental data.
#'
#' @param object The \code{env.cont} object read in by \link{envDataRead}.
#' @param short Should the output be truncated to the first elements?
#'   (default: TRUE) This is useful when the dataset is large.
#' @param ... ignored
#'
#' @return list with:
#'   \itemize{
#'     \item class - the full class name;
#'     \item nrow - number of rows;
#'     \item ncol - number of columns;
#'     \item rownames - character vector with row names;
#'     \item colnames - character vector with column names.
#'   }
#'
#' @export
summary.env.cont <- function( object, short = TRUE, ... ){
	summary.env.data( object, short = short, cont = TRUE )
}

#' Info about the \code{env.cat} class
#'
#' Prints a short summary about the categorical environmental data.
#'
#' @param object The \code{env.cat} object read in by \link{envDataRead}.
#' @param short Should the output be truncated to the first elements?
#'   (default: TRUE) This is useful when the dataset is large.
#' @param ... ignored
#'
#' @return list with:
#'   \itemize{
#'     \item class - the full class name;
#'     \item nrow - number of rows;
#'     \item ncol - number of columns;
#'     \item rownames - character vector with row names;
#'     \item colnames - character vector with column names.
#'   }
#'
#' @export
summary.env.cat <- function( object, short = TRUE, ... ){
	summary.env.data( object, short = short, cont = FALSE )
}
