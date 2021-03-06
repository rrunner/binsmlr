#' binsmlr
#'
#' \strong{binsmlr}, as in \emph{bin similar}, is a package to create bins that
#' are assessed to be heterogenous (in terms of default risk) across a range of
#' ordinal values. The binning can be performed automatically, interactively and
#' manually.
#'
#' The objective is to create groups of data (bins) across a range of ordinal
#' values. The bins produced should be homogenous within and heterogenous across
#' bins in terms of the default risk. A binning approach, in the context of data
#' transformation, is common in different credit risk applications and in
#' particular to credit risk portfolios having a relatively large number of
#' defaults. A prerequisite is that each observation is assigned an ordinal
#' value such as score or rating etc., that indicates the relative default risk
#' as proposed by the rank ordering model. Also, a default indicator is
#' required.
#'
#' The main process is the following:
#' \enumerate{
#' \item Create a relatively large set of initial bins.
#' \item Perform homogeneity tests between all adjacent bins
#' \item Merge the pair of bins that are the most similar in terms
#' of proportions of state of non-default and state of default.
#' \item Repeat step 2 and 3 until all bins are heterogeneous or if a minimum
#' number of bins are obtained.
#' }
#'
#' The process above should result in bins that are statistically different in
#' terms of default risk across a range of ordinal data.
#'
#' The binsmlr package provides the following functionality as listed below.
#'
#' @section Automatic binning: See \code{\link{autobin}} for a fully automatic
#'   binning approach.
#'
#' @section Interactive binning: See \code{\link{create_initial_bins}} and
#'   \code{\link{reduce_bins}} for how to perform binning in a slightly more
#'   interactive way.
#'
#' @section Manual binning: See \code{\link{merge_list_of_bins}} on how to
#'   perform binning manually once the basic data structure is in place.
#'
#' @docType package
#' @name binsmlr
NULL

#' Ordinal value and default indicator data
#'
#' A dataset containing the scores and default indicator of 200,000 obligors.
#'
#' @format A data frame with 200,000 rows and 2 variables:
#' \describe{
#'   \item{score}{the score is the ordinal value that indicates the relative
#' default risk of the obligor. The higher the score, the lower is the predicted
#' default risk as proposed by the rank ordering model}
#'   \item{default}{a default indicator where 1 implies default status (in the
#' next 12-month period) and 0 indicates non-default status}
#' }
"bin_data"
