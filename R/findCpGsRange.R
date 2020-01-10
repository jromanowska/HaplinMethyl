#' Finding CpGs within a given range
#'
#' This is a basic function for searching for CpGs within a given coordinate range
#'
#' Given a position range and chromosome number, along with a table of positions
#'   of CpGs in this chromosome, the function will return a data.frame with
#'   the chosen CpGs.
#'
#' @param range A vector with start and end position of the range.
#' @param cpgs A table with all CpGs in a given chromosome, with the columns
#'     named: "id", "coord".
#'
#' @return A data.frame with CpGs with columns named: "id", "coord".
#'
#' @export
#'
findCpGsRange <- function( range,
						   cpgs ){
	if( length( range ) != 2 ){
		stop( "The given 'range' must be a vector with exactly two elements:
			  the beginning and end of the coordinate range.", call. = FALSE )
	}
	if( names( cpgs ) != c( "id", "coord" ) ){
		stop( "The given 'cpg' must be a data.frame with the columns named 'id' and 'coord'!",
			  call. = FALSE )
	}

	close.cpgs <- which( cpgs$coord < max( range ) & cpgs$coord > min( range ) )
	if( length( close.cpgs ) == 0 ){
		message( "No close CpGs within the range: ",
				 paste0( range, collapse = " - " ), ".\n" )
		return( NULL )
	}

	return( data.frame( id = cpgs$id[ close.cpgs ],
						coord = cpgs$coord[ close.cpgs ] ) )
}
