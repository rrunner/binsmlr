# function to prepare data
# - aggregate on the ordinal value ('group_by' also implies sorting)
# - count the number of defaults and the total number of obligors
# - select/rename the variables
#' @importFrom rlang .data
prepare_data <- function(data, ordinal_value, default_ind) {

    # hack to pass CRAN check
    num_defaults <- num_obligors <- score <- NULL

    d <- dplyr::group_by(data, .data[[ordinal_value]])
    d <- dplyr::summarise(
                       d,
                       num_defaults = sum(.data[[default_ind]]),
                       num_obligors = dplyr::n(),
                       .groups = "drop"
                       )
    d <- dplyr::select(d,
                       score = .data[[ordinal_value]],
                       num_defaults,
                       num_obligors)
    dplyr::arrange(d, score)
}

#' Create initial bins
#'
#' Creates a set of initial bins.
#'
#' The current (only) method is based on an 'equal height' approach where a new
#' bin is created as soon as the number of defaults is equal to (or larger) than
#' a user defined \code{threshold} level. Thus, the term 'equal height' refers
#' to roughly equal number of defaults in each bin. The data is processed based
#' on the user supplied variables holding the \code{ordinal_value} and the
#' \code{default_ind}. The state of default in \code{default_ind} is generally 1
#' if the obligor defaults in the next 12-month period. To avoid bias in the
#' default rate calculation, only current non-default obligors should be
#' included in \code{data}. This type of data pre-processing is left to the user
#' before calling \code{\link{create_initial_bins}}. Similary, the rank-order
#' information in \code{ordinal_value} must be invariant to sorting. Thus, it
#' may be required to convert characters/factors of ratings to numerical values
#' before calling \code{\link{create_initial_bins}}.
#'
#' @param data The input data that contains \code{ordinal_value} and
#'   \code{default_ind}.
#' @param threshold An integer that defines the minimum number of defaults in
#'   each bin. Typically needs a reasonable number of defaults to ensure the
#'   homogeneity test can be executed with some power.
#' @param ordinal_value A numeric ordinal value that rank order each obligor's
#'   relative default risk. For example, a numeric score or numeric rating
#'   information.
#' @param default_ind An integer, or double, representing the default status.
#'   The state of default must be indicated by 1 and non-default by 0.
#'
#' @return A list of bins. Each list component in the returned list is a bin (of
#'   class bin). The list is returned in ascending order by min_score (see link
#'   to constructor below), and each bin contains at least \code{threshold}
#'   number of defaults.
#'
#' @seealso The (helper) constructor to bin objects, \code{\link{bin}}. See also
#'   \code{\link{reduce_bins}}.
#'
#' @export
#'
#' @examples
#' # create a set of initial bins
#' bins <- create_initial_bins(bin_data, 30, "score", "default")
#' bins_df <- dplyr::bind_rows(bins)
#' plot(x = bins_df$mid_score, y = log(bins_df$odds), type = "p",
#'      col = "lightblue", cex = 1.5, pch = 20,
#'      ylab = "log(odds)", xlab = "score")
create_initial_bins <- function(data = NULL,
                                threshold = NULL,
                                ordinal_value = NULL,
                                default_ind = NULL) {

  # prepare data
  validate_input_data(data, default_ind)
  df <- prepare_data(data, ordinal_value, default_ind)

  # validate input data
  threshold <- as.integer(threshold)
  validate_prepared_data(df, threshold)

  # initialize
  def <- obl <- 0
  bin_mins <- Inf
  bin_maxs <- -Inf
  highest_score <- max(df$score)
  # upper bound of maximum number of bins is nrow(df)
  lst <- vector(mode = "list", length = nrow(df))

  # iterate trough the entire (pre-sorted) data frame
  for (i in seq_len(nrow(df))) {
    current_row <- df[i, ]
    current_score <- current_row$score

    def <- def + current_row$num_defaults
    obl <- obl + current_row$num_obligors

    bin_mins <- min(bin_mins, current_score)
    bin_maxs <- max(bin_maxs, current_score)

    # create a new bin when default threshold is reached or when
    # evaluating the last score
    if (def >= threshold || current_score == highest_score) {

      # next element to populate (first NULL position)
      nxt <- Position(is.null, lst)

      lst[[nxt]] <- bin(
        num_obl   = obl,
        num_def   = def,
        min_score = bin_mins,
        max_score = bin_maxs
      )

      def <- obl <- 0
      bin_mins <- Inf
      bin_maxs <- -Inf

      # merge the last two bins if the last score is processed and
      # the last bin contains fewer defaults than the threshold level
      if (current_score == highest_score && lst[[nxt]]$num_def < threshold) {

        lst <- merge_list_of_bins(lst, (nxt - 1):nxt)
      }
    }
  }

  # keep only non NULL list elements
  Filter(Negate(is.null), lst)
}
