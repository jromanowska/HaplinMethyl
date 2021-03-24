#' Finding CpGs near a SNP
#'
#' This is a basic function for searching for CpGs in the vicinity of a given SNP
#'
#' Given a position and chromosome number of a SNP, along with a table of
#'   positions of CpGs in this chromosome, the function will return a data.frame
#'   with nearby CpGs (within a given limit).
#'
#' @param snp A named list with SNP name ("marker"), chromosome no. ("chr")
#'     and coordinate ("coord").
#' @param cpgs A table with all CpGs in a given chromosome, with the columns
#'     named: "id", "coord".
#' @param range Number giving the maximum distance from the SNP where the
#'    function will look for CpGs; default: 5000 (base pairs).
#' @param verbose Whether to display extra info about each SNP (default: FALSE).
#'
#' @return A data.frame with CpGs with columns named: "id", "coord".
#'
#' @export
#'
findCpGsnearSNP <- function( snp = stop( "'snp' is required!", call. = FALSE ),
							 cpgs = stop( "'cpgs' is required!", call. = FALSE ),
							 range = 5000,
							 verbose = FALSE
){
	if( length( snp ) < 3 ){
		stop( "The given 'snp' must be a named list,
			  including the marker name ('marker'), chromosome number ('chr'),
			  and the coordinate ('coord').", call. = FALSE )
	}
  if( is.recursive( cpgs ) == FALSE ){
    stop( "The given argument 'cpgs' is not compatible with the $ operator. 
          'cpgs' must be a recursive (list-like) object such as a data frame, 
          list or tibble."
          , call. = FALSE )
  }
	if( !all(names( cpgs ) %in% c( "id", "coord" )) ){
		stop( "The given 'cpgs' must be a data.frame with the columns named 'id' and 'coord'!",
			  call. = FALSE )
	}

	close.cpgs <- which( cpgs$coord < snp$coord + range &
						 	cpgs$coord > snp$coord - range )
	if( length( close.cpgs ) == 0 ){
		if( verbose ){
			message( paste0(
				"No close CpGs within the range: ", range, " from SNP: ",
				snp$marker, ", position ", snp$coord,", at chromosome ",
				snp$chr, ".\n" ) )
		}
		return( NULL )
	}

	return( data.frame( id = cpgs$id[ close.cpgs ],
						coord = cpgs$coord[ close.cpgs ] ) )
}
