# default rate
dr <- function(def, total) {
  def / total
}

# odds ratio
odds <- function(dr) {
  dr / (1 - dr)
}

# mid score
midscore <- function(mns, mxs) {
  (mns + mxs) / 2
}

# constructor
new_bin <- function(num_obl = integer(),
                    num_def = integer(),
                    min_score = double(),
                    max_score = double(),
                    is_merged = FALSE) {
  stopifnot(
    purrr::is_integer(num_obl),
    purrr::is_integer(num_def),
    purrr::is_double(min_score),
    purrr::is_double(max_score),
    purrr::is_logical(is_merged)
  )

  default_rate <- dr(num_def, num_obl)
  odds_ratio   <- odds(default_rate)

  bin <- list(
    num_obl   = num_obl,
    num_def   = num_def,
    dr        = default_rate,
    odds      = odds_ratio,
    min_score = min_score,
    mid_score = midscore(min_score, max_score),
    max_score = max_score,
    is_merged = is_merged
  )

  structure(bin, class = c("bin", "list"))
}

#' A constructor to bin objects
#'
#' A (helper) constructor to create objects of class bin. Each bin object
#' inherits from the class list.
#'
#' Validation is performed on the arguments before passing on the values to the
#' constructor. Also, a print method exists for bin objects.
#'
#' @param num_obl An integer representing the number of obligors (customers).
#'   This value must include both defaults and non-defaults.
#' @param num_def An integer representing the number of defaults. The defaults
#'   is the stock of obligors that are (currently) in non-default state but
#'   defaults at least once in the next 12-month period (typically).
#' @param min_score A double representing the minimum score.
#' @param max_score A double representing the maximum score.
#' @param ... Optional parameters passed on to the constructor. Used internally
#'   to set is_merged to TRUE as part of the merge functionality.
#'
#' @return An object of class bin. The structure of each bin is the
#'   following
#'   \describe{
#'     \item{num_obl}{The number of obligors (customers).}
#'     \item{num_def}{The number of defaults.}
#'     \item{dr}{The default rate.}
#'     \item{odds}{The odds ratio.}
#'     \item{min_score}{The minimum score.}
#'     \item{mid_score}{The midpoint score (simple average of \code{min_score}
#'       and \code{max_score}).}
#'     \item{max_score}{The maximum score.}
#'     \item{is_merged}{A boolean. TRUE indicates the bin has been merged with
#'   other bins. FALSE implies the bin is identical to the initialized bin. The
#'   merged status is visible in the print() output of bin objects.}
#'   }
#'
#' @export
#'
#' @examples
#' # create a single bin object
#' single_bin <- bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60)
#' print(single_bin)
#'
#' # create a list of bins
#' bins <- list(
#'           bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60),
#'           bin(num_obl = 200, num_def = 5, min_score = 61, max_score = 80)
#'           )
#' print(bins)
bin <- function(num_obl = integer(),
                num_def = integer(),
                min_score = double(),
                max_score = double(), ...) {

  num_obl   <- as.integer(num_obl)
  num_def   <- as.integer(num_def)
  min_score <- as.double(min_score)
  max_score <- as.double(max_score)

  validate_new_bin(new_bin(num_obl, num_def, min_score, max_score, ...))
}
