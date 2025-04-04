% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findCpGsnearSNP.R
\name{findCpGsnearSNP}
\alias{findCpGsnearSNP}
\title{Finding CpGs near a SNP}
\usage{
findCpGsnearSNP(
  snp = stop("'snp' is required!", call. = FALSE),
  cpgs = stop("'cpgs' is required!", call. = FALSE),
  range = 5000,
  verbose = FALSE
)
}
\arguments{
\item{snp}{A named list (or one-row data.frame) containing the SNP's
information. The object must contain three elements (or columns):\cr
\code{"marker"} (a character string containing the SNP's name/ID),\cr
\code{"chr"} (an integer representing the chromosome number), and\cr
\code{"coord"} (an integer representing the SNP's position on \code{chr}).}

\item{cpgs}{A named list (or a data.frame with one row per CpG) containing
the ID(s) and coordinate(s) of all the CpGs on the chromosome where the SNP
is located.\cr
Must contain only two elements/columns:\cr
\code{"id"} (containing unique CpG locus cluster IDs; cg#), and\cr
\code{"coord"} (containing integers representing the coordinates of the
cytosine nucleotides of the CpG loci).}

\item{range}{An integer specifying the desired maximum number of base pairs
between the given SNP and CpGs. The default is 5000. The search for
\code{cpgs$coord} is restricted to the open interval \code{(snp$coord - range,
  snp$coord + range)}.}

\item{verbose}{Whether to print information about the result when available
(default: \code{FALSE}).}
}
\value{
A data.frame with columns named \code{id} and \code{coord},
containing one row per CpG that is 'nearby' the SNP. Returns \code{NULL} if
none of the CpGs in \code{cpgs} are within the given range of the SNP.
}
\description{
This is a basic function for searching for CpGs in the vicinity
of a given SNP.
}
\details{
Given the position and chromosome number of a particular SNP, along
with the positions of CpGs on the same chromosome, the function will return
a data.frame with nearby CpGs (within a specified number of base pairs).

The positions of the CpGs should refer to the cytosine nucleotide of the
CpG (or CpH) dinucleotide.

A CpG is classified as 'near' the SNP if the position of its cytosine
nucleotide falls within the following open interval: \verb{(snp$coord - range , snp$coord + range)}.

This function uses the 1-based coordinate system.

\strong{Please note} that you should ensure that every CpG in \code{cpgs} and the SNP
in \code{snp} are located on the same chromosome.
}
\examples{
# Named list with SNP information:
snp_list <- list(marker = "rs123", chr = 1, coord = 1000000)

# Data frame with CpGs:
cpgs_df <- data.frame( id = c("cg001", "cg002", "cg003"),
                       coord = c(999000, 1000500, 1005000))

# Find CpGs within 5000 bp of SNP
findCpGsnearSNP(snp = snp_list, cpgs = cpgs_df)

}
