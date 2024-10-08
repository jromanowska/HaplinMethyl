% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envDataCategorize.R
\name{envDataCategorize}
\alias{envDataCategorize}
\title{Creating categorical variables out of continuous data}
\usage{
envDataCategorize(
  env.data = stop("You didn't provide the environmental data!", call. = FALSE),
  summary.method = "sum",
  breaks,
  file.out = "env_data_cat",
  dir.out = ".",
  overwrite = NULL
)
}
\arguments{
\item{env.data}{The environmental data, read in by \link{envDataRead}
function.}

\item{summary.method}{If there are more than one probe (rows), this method is
used to summarize the continuous data across columns to create one number
per row (sample), which will be then used to categorize data.}

\item{breaks}{Numerical vector indicating how to divide the continuous values
into categories (see Details).}

\item{file.out}{The core name of the files that will contain the categorized
data (character string); ready to load next time with \link{envDataLoad}
function; default: "env_data_cat".}

\item{dir.out}{The directory that will contain the saved data; defaults to
current working directory.}

\item{overwrite}{Whether to overwrite the output files: if NULL (default),
will prompt the user to give answer; set to TRUE, will automatically
overwrite any existing files; and set to FALSE, will stop if the output
files exist.}
}
\value{
A list of ff matrices, now containing the categorized data (factors).
The function also creates two files: .RData and .ffData.
}
\description{
This function prepares the environmental data to be used in
stratification when calling \link{haplinStrat}.
}
\section{Details}{

The \code{env.data} given here is assumed to be a set that is somehow linked to
each other, e.g., if the data is DNA methylation measurements for various
CpGs, the CpGs might be from one region around a given SNP.

The \code{summary.method} argument takes a value from the following list:
\itemize{
\item \code{sum} - arithmetical sum of the values (default);
\item \code{average} - average value;
\item \code{NULL} - no summary; \strong{NOTE:} if the data contains more than one
column, be sure to check that the breaks give correct division for
each of the column!
}

When \code{breaks} is one number, it defines the number of categories that the
range of values will be divided into. The categories will be equal in size,
based on the appropriate quantiles calculated from the summarized values.
}

