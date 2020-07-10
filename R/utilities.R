#' Verify monotonicity condition of default rates
#'
#' Verify if a list of bins is monotonic in terms of default rates. The function
#' \code{is_monotonic} returns a boolean indicating TRUE if the default rates
#' exhibit a monotonic trend.
#'
#' A list of bins of length N contains N default rates. The monotonic condition
#' tested by this function examines whether the N default rates decrease (or
#' increase) for each consecutive default rate in the list. The test is
#' non-strict, meaning that if the default rates between any two adjacent bins
#' are equal this is not considered a breach of monotonicity.
#'
#' @param list_of_bins A list of bins. Every bin object contains a default rate.
#' @param decreasing A boolean. Should the monotonicity test verify a decrease
#'   or increase in default rates?
#' @param verbose A boolean. If set to TRUE, prints a matrix of default rates
#'   along with a binary indicator displaying 1 where the monotonic
#'   relationships fails to hold.
#'
#' @return A boolean. TRUE indicates a monotonic development in default rates
#'   across all bins in \code{list_of_bins}.
#'
#' @export
#'
#' @examples
#' # create a set of initial bins and verify where monotonicity fails
#' bins <- create_initial_bins(bin_data, 30, "score", "default")
#' is_monotonic(bins, verbose = TRUE)
is_monotonic <- function(list_of_bins, decreasing = TRUE, verbose = FALSE) {

  default_rates <- purrr::map_dbl(list_of_bins, "dr")

  if (decreasing) {
    check <- default_rates == cummin(default_rates)
  } else {
    check <- default_rates == cummax(default_rates)
  }
  is_monotonic <- all(check)

  if (verbose) {
    print(
      cbind(default_rates = default_rates, fail_monotonicity = !check)
    )
  }

  is_monotonic
}

# between in an open interval (strictly in between)
strictly_between <- function(x, left, right) {
  x > left & x < right
}
