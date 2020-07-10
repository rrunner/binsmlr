# specific merge operations relevant to 'list of bins'
merge_bins_operations <- function(list_of_bins) {

  bin(
    num_obl   = sum(purrr::map_int(list_of_bins, "num_obl")),
    num_def   = sum(purrr::map_int(list_of_bins, "num_def")),
    min_score = min(purrr::map_dbl(list_of_bins, "min_score")),
    max_score = max(purrr::map_dbl(list_of_bins, "max_score")),
    is_merged = TRUE
    )
}

#' Merge list of bins
#'
#' A merge function to collapse bin objects. This function is used by other
#' functions in the binsmlr package, but it is also useful on a standalone basis
#' when manual binning is required (enforce monotonicity across bins in terms of
#' default rates).
#'
#' @param list_of_bins A list of bins.
#' @param idx An integer (index) vector indicating which bins in
#'   \code{list_of_bins} to merge. The index vector must be sorted in ascending
#'   order without any gaps.
#'
#' @return A list of bins. Each list component in the returned list is a bin (of
#'   class bin). Total length equal to \code{length(list_of_bins) - length(idx)
#'   + 1}.
#'
#' @export
#'
#' @examples
#' # example of manual binning
#' autobins <- autobin(bin_data, 30, "score", "default", 12, 0.01)
#' length(autobins)
#' is_monotonic(autobins, verbose = TRUE)
#' man_bins <- merge_list_of_bins(autobins, 3:4)
#' length(man_bins)
#' is_monotonic(man_bins)
merge_list_of_bins <- function(list_of_bins, idx) {

  # validation of index vector
  validate_merge_list_of_bins(idx)

  # apply merge operations
  merged_bin <- merge_bins_operations(list_of_bins[idx])

  # remove the bins that have been merged
  list_of_bins[idx] <- NULL

  # insert new merged bin into the list of bins
  append(list_of_bins, list(merged_bin), after = min(idx) - 1)
}

# function to compute the test statistics of independence between a pair of bins
homogeneity_test <- function(bin1 = NULL,
                             bin2 = NULL,
                             test_type = c("chisq.test", "fisher.test")) {

  tab <- rbind(
    c(bin1$num_obl, bin2$num_obl),
    c(bin1$num_def, bin2$num_def)
  )

  dimnames(tab) <- list(
    status = c("performing", "default")
  )

  test_type <- match.arg(test_type)

  # Sample from two different bins, each having two statuses of either
  # non-default/default results in (2-1) x (2-1) = 1 d.o.f.
  # - H0: no difference in proportion of non-defaults/defaults between two bins
  # - H1: difference in proportion of non-defaults/defaults between two bins
  test_result <- switch(
    test_type,
    chisq.test = stats::chisq.test(tab),
    fisher.test = stats::fisher.test(tab)
  )

  p_val <- as.numeric(test_result["p.value"])

  p_val
}

# wrapper function to perform homogeneity tests in parallel
update_homogeneity_tests <- function(list_of_bins, test_type) {

  purrr::map2_dbl(
    list_of_bins[1:(length(list_of_bins) - 1)],
    list_of_bins[2:length(list_of_bins)],
    homogeneity_test,
    test_type
    )
}

#' Reduce the number of bins
#'
#' Sequentially reduce the number of bins (from a list of bins) based on
#' similarity in terms of proportions of non-default/default status between
#' adjacent bins.
#'
#' Similarity, or homogeneity, is assessed by performing a test of independence.
#' The list of bins are reduced by merging the most similar pair of adjacent
#' bins. The function terminates when a minimum number of required of bins are
#' obtained or when all the bins are statistically different (heterogeneous) at
#' the given level of confidence.
#'
#' The returned list of bins is not guaranteed to exhibit a monotonic
#' development of default rates, but it is likely that the sequential reduction
#' of bins will mitigate the problem. Should monotonicity be required, see
#' \code{\link{merge_list_of_bins}} on how to impose a manual binning approach
#' as a final step.
#'
#' @param list_of_bins A list of bins.
#' @param min_required_bins An integer (minimum two). The minimum number of bins
#'   in the returned list.
#' @param confidence_level A double between 0 and 1 representing the confidence
#'   level passed onto the homogeneity test.
#' @param test_type The type of homogeneity test,
#'   \code{\link[stats]{chisq.test}} or \code{\link[stats]{fisher.test}}.
#'   Defaults to the former.
#'
#' @return A list of bins. Each list component in the returned list is a bin (of
#'   class bin).
#'
#' @seealso See \code{\link{create_initial_bins}} on how to create the initial
#'   bins, \code{\link{merge_list_of_bins}} for manual binning, and
#'   \code{\link{autobin}} for automatic binning.
#'
#' @export
#'
#' @examples
#' # example of interactive binning
#'
#' # create initial bins
#' bins <- create_initial_bins(bin_data, 30, "score", "default")
#' length(bins)
#' is_monotonic(bins)
#'
#' # reduce bins by performing repeated homogeneity tests
#' new_bins <- reduce_bins(bins, min_required_bins = 7, confidence_level = 0.01)
#' length(new_bins)
#' is_monotonic(new_bins)
#'
#' # plot initial and reduced bins
#' bins_df <- dplyr::bind_rows(bins)
#' plot(x = bins_df$mid_score, y = log(bins_df$odds), type = "p",
#'      col = "lightblue", cex = 1.5, pch = 20, ylab = "log(odds)",
#'      xlab = "score")
#'
#' new_bins_df <- dplyr::bind_rows(new_bins)
#' points(x = new_bins_df$mid_score, y = log(new_bins_df$odds),
#'        col = "darkblue", cex = 1.5, pch = 20)
#' legend(x = "topright", legend = c("Initial bins", "Reduced bins"),
#'        col = c("lightblue", "darkblue"), pch = 20, pt.cex = 1.5, bty = "n")
reduce_bins <- function(list_of_bins = NULL,
                        min_required_bins = NULL,
                        confidence_level = NULL,
                        test_type = "chisq.test") {

  # validate input arguments
  min_required_bins <- as.integer(min_required_bins)
  validate_reduce_bins(min_required_bins, confidence_level)

  # initiate vector of p-values based on homogeneity test
  pvalues <- update_homogeneity_tests(list_of_bins, test_type)

  while (TRUE) {

    # test for minimum number of required bins
    len_lst <- length(list_of_bins)
    if (len_lst <= min_required_bins) {
      message(
        "Minimum number of required bins is obtained",
        "\nThere are in total ", len_lst, " number of bins",
        "\nBinning completed!\n"
      )

      break
    }

    # find the two 'most homogenous' adjacent bins according to test
    idx_max <- which.max(pvalues)
    pval_max <- pvalues[idx_max]

    # test if all bins are statistically different (at chosen confidence level)
    if (pval_max <= confidence_level) {
      message(
        strwrap("All adjacent bin pairs are now statistically different in terms
                 of default rates"),
        "\nThe test of independence returns a p-value of: ", round(pval_max, 6),
        "\nThere are in total ", len_lst, " number of bins",
        "\nBinning completed!\n"
      )

      break
    }

    # merge the two 'most homogenous' adjacent bins
    bin_pair_to_merge <- c(idx_max, idx_max + 1)
    list_of_bins <- merge_list_of_bins(list_of_bins, bin_pair_to_merge)

    # update homogeneity tests after merge
    pvalues <- update_homogeneity_tests(list_of_bins, test_type)
  }

  list_of_bins
}

#' Automatic binning
#'
#' Automatic binning based on similarity in terms of proportions of
#' non-default/default status between adjacent bins. \code{\link{autobin}}
#' simply combines \code{\link{create_initial_bins}} and
#' \code{\link{reduce_bins}}.
#'
#' @inheritParams create_initial_bins
#' @inheritParams reduce_bins
#' @param ... Optional parameters passed on to \code{reduce_bins}.
#'
#' @return A list of bins. Each list component in the returned list is a bin (of
#'   class bin).
#'
#' @export
#'
#' @seealso See \code{\link{create_initial_bins}}, \code{\link{reduce_bins}} and
#'   \code{\link{merge_list_of_bins}}.
#'
#' @examples
#' # example of automatic binning
#' autobins <- autobin(bin_data, 30, "score", "default", 7, 0.01)
#' bins_df <- dplyr::bind_rows(autobins)
#' plot(x = bins_df$mid_score, y = log(bins_df$odds), type = "p",
#'      col = "lightblue", cex = 1.5, pch = 20, ylab = "log(odds)",
#'      xlab = "score")
autobin <- function(data,
                    threshold,
                    ordinal_value,
                    default_ind,
                    min_required_bins,
                    confidence_level, ...) {

  init_bins <- create_initial_bins(data, threshold, ordinal_value, default_ind)
  reduce_bins(init_bins, min_required_bins, confidence_level, ...)
}
