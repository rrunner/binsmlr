% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binning.R
\name{autobin}
\alias{autobin}
\title{Automatic binning}
\usage{
autobin(
  data,
  threshold,
  ordinal_value,
  default_ind,
  min_required_bins,
  confidence_level,
  ...
)
}
\arguments{
\item{data}{The input data that contains \code{ordinal_value} and
\code{default_ind}.}

\item{threshold}{An integer that defines the minimum number of defaults in
each bin. Typically needs a reasonable number of defaults to ensure the
homogeneity test can be executed with some power.}

\item{ordinal_value}{A numeric ordinal value that rank order each obligor's
relative default risk. For example, a numeric score or numeric rating
information.}

\item{default_ind}{An integer, or double, representing the default status.
The state of default must be indicated by 1 and non-default by 0.}

\item{min_required_bins}{An integer (minimum two). The minimum number of bins
in the returned list.}

\item{confidence_level}{A double between 0 and 1 representing the confidence
level passed onto the homogeneity test.}

\item{...}{Optional parameters passed on to \code{reduce_bins}.}
}
\value{
A list of bins. Each list component in the returned list is a bin (of
  class bin).
}
\description{
Automatic binning based on similarity in terms of proportions of
non-default/default status between adjacent bins. \code{\link{autobin}}
simply combines \code{\link{create_initial_bins}} and
\code{\link{reduce_bins}}.
}
\examples{
# example of automatic binning
autobins <- autobin(bin_data, 30, "score", "default", 7, 0.01)
bins_df <- dplyr::bind_rows(autobins)
plot(x = bins_df$mid_score, y = log(bins_df$odds), type = "p",
     col = "lightblue", cex = 1.5, pch = 20, ylab = "log(odds)",
     xlab = "score")
}
\seealso{
See \code{\link{create_initial_bins}}, \code{\link{reduce_bins}} and
  \code{\link{merge_list_of_bins}}.
}
