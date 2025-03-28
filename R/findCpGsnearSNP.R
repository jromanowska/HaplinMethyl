#' Finding CpGs near a SNP
#'
#' @description This is a basic function for searching for CpGs in the vicinity
#'   of a given SNP.
#'
#' @details Given the position and chromosome number of a particular SNP, along
#'   with the positions of CpGs on the same chromosome, the function will return
#'   a data.frame with nearby CpGs (within a specified number of base pairs).
#'
#'   The positions of the CpGs should refer to the cytosine nucleotide of the
#'   CpG (or CpH) dinucleotide.
#'
#'   A CpG is classified as 'near' the SNP if the position of its cytosine
#'   nucleotide falls within the following open interval: `(snp$coord - range ,
#'   snp$coord + range)`.
#'
#'   This function uses the 1-based coordinate system.
#'
#' **Please note** that you should ensure that every CpG in `cpgs` and the SNP
#'   in `snp` are located on the same chromosome.
#'
#' @param snp A named list (or one-row data.frame) containing the SNP's
#'   information. The object must contain three elements (or columns):\cr
#'   \code{"marker"} (a character string containing the SNP's name/ID),\cr
#'   \code{"chr"} (an integer representing the chromosome number), and\cr
#'   \code{"coord"} (an integer representing the SNP's position on \code{chr}).
#' @param cpgs A named list (or a data.frame with one row per CpG) containing
#'   the ID(s) and coordinate(s) of all the CpGs on the chromosome where the SNP
#'   is located.\cr
#'   Must contain only two elements/columns:\cr 
#'   \code{"id"} (containing unique CpG locus cluster IDs; cg#), and\cr 
#'   \code{"coord"} (containing integers representing the coordinates of the 
#'   cytosine nucleotides of the CpG loci).
#' @param range An integer specifying the desired maximum number of base pairs
#'   between the given SNP and CpGs. The default is 5000. The search for
#'   `cpgs$coord` is restricted to the open interval \code{(snp$coord - range,
#'   snp$coord + range)}.
#' @param verbose Whether to print information about the result when available
#'   (default: \code{FALSE}).
#'
#' @return A data.frame with columns named \code{id} and \code{coord},
#'   containing one row per CpG that is 'nearby' the SNP. Returns \code{NULL} if
#'   none of the CpGs in \code{cpgs} are within the given range of the SNP.
#'
#' @examples
#' # Named list with SNP information:
#' snp_list <- list(marker = "rs123", chr = 1, coord = 1000000)
#'
#' # Data frame with CpGs:
#' cpgs_df <- data.frame( id = c("cg001", "cg002", "cg003"),
#'                        coord = c(999000, 1000500, 1005000))
#'
#' # Find CpGs within 5000 bp of SNP
#' findCpGsnearSNP(snp = snp_list, cpgs = cpgs_df)
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
	  stop( paste( "The given 'cpgs' must be a list or data.frame with exactly 2",
	               "elements or columns named 'id' and 'coord'!"),
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
