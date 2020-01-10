#' HaplinMethyl: Additional functions for Haplin, for using matrices of
#'   environmental data (e.g., DNA methylation data)
#'
#' The HaplinMethyl package provides three categories of important functions:
#' reading the data, loading the read-in data, and manipulating the
#' environmental data.
#'
#' @section Reading the environmental data:
#' The two functions (\link{envDataRead} and \link{envDataReadFromObj}) read in
#'   the data and create an object of \code{env.data} class, which is basically
#'   a list with ff-matrices. Simultaneously, two files are written: \code{.RData}
#'   and \code{.ffData}. These are connected and will be used in the data-loading
#'   function. The object created here has also \link{summary} and \link{print}
#'   methods defined for easier viewing.
#'
#' @section Loading the read-in data:
#' \link{envDataLoad} function loads the data, which is super-fast, compared to
#'   reading the data in.
#'
#' @section Manipulating the data:
#' The user can extract a subset of the data read in by any of the above
#'  functions, with \link{envDataSubset}. This will create new \code{.RData} and
#'  \code{.ffData} files. Moreover, there are two functions that are created
#'  specifically for DNA methylation data: \link{findCpGsnearSNP} and
#'  \link{findCpGsRange} can be used to create CpG lists that are near a given
#'  SNP or within a certain coordinate range.
#'
#' @docType package
#' @name HaplinMethyl
#'
#' @import Haplin
NULL
