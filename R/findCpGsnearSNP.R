#' Finding CpGs near a SNP
#'
#' @description 
#' This is a basic function for searching for CpGs in the vicinity of a given
#' SNP.
#'
#' @details 
#' Given the position and chromosome number of a SNP, along with the positions
#' of CpGs on the same chromosome, the function will return a data.frame with
#' nearby CpGs (within a given number of base pairs).
#'  
#' **Please note** that you should ensure that every CpG in `cpgs` and the SNP
#'  in `snp` are located on the same chromosome.
#'
#' @param snp A named list with the SNP's name ("marker"), chromosome no. ("chr")
#'     and coordinate ("coord").
#' @param cpgs A list or data.frame containing all CpGs on the chromosome where
#'   the SNP is located. Must contain only two elements/columns: one named "id" 
#'   (unique CpG locus cluster ID, cg#) and one named "coord" (coordinate of CpG
#'    locus).
#' @param range An integer specifying the desired maximum number of base pairs
#'   between the given SNP and CpGs. The default is 5000. The search for CpGs is
#'   restricted to the interval (snp$coord - range, snp$coord + range).
#' @param verbose Whether to print information about the result when available
#'   (default: FALSE).
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
	if( !( length(names( cpgs )) == 2)  ){
		stop( paste( "The given 'cpgs' must be a list or data.frame with exactly 2",
		             "elements or columns!"),
			  call. = FALSE )
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
