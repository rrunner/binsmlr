# validate objects of class bin created by new_bin()
validate_new_bin <- function(x) {

  if (x[["num_obl"]] <= 0) {
    stop(
      "Number of obligors must be a positive integer",
      call. = FALSE
    )
  }

  if (x[["num_def"]] < 0) {
    stop(
      "Number of defaults must be a non-negative integer",
      call. = FALSE
    )
  }

  if (x[["num_obl"]] < x[["num_def"]]) {
    stop(
      "Number of obligors is less than the number of defaults",
      call. = FALSE
    )
  }

  if (!dplyr::between(x[["dr"]], 0, 1)) {
    stop(
      "Default rate must be in the closed interval [0, 1]",
      call. = FALSE
    )
  }

  if (!(x[["odds"]] >= 0)) {
    stop(
      "Odds ratio cannot be negative",
      call. = FALSE
    )
  }

  if (x[["min_score"]] > x[["max_score"]]) {
    stop(
      "min_score cannot exceed max_score",
      call. = FALSE
    )
  }

  if (!dplyr::between(x[["mid_score"]], x[["min_score"]], x[["max_score"]])) {
    stop(
      "mid_score must be within min_score and max_score",
      call. = FALSE
    )
  }

  if (!purrr::is_logical(x[["is_merged"]])) {
    stop(
      "is_merged must be logical, TRUE or FALSE",
      call. = FALSE
    )
  }

  x
}

# validate the indices in merge_list_of_bins()
validate_merge_list_of_bins <- function(index) {

  idx_test <- min(index):max(index)

  if (any(index <= 0)) {
    stop(
      "Index values must be positive",
      call. = FALSE
    )
  }

  if (length(index) < 2) {
    stop(
      "Length of index vector shorter than two",
      call. = FALSE
    )
  }

  if (!(all(sort(index) == index))) {
    stop(
      "Index vector is not in ascending sorting order",
      call. = FALSE
    )
  }

  if (!(length(idx_test) == length(index))) {
    stop(
      "Index values are missing",
      call. = FALSE
    )
  }
}

# validate the input data
#' @importFrom rlang .data
validate_input_data <- function(input_data, def_ind) {

  if (!(purrr::is_integer(input_data[, def_ind, drop = TRUE]) ||
        purrr::is_double(input_data[, def_ind, drop = TRUE]))) {
    stop(
      "'default_ind' must be of type integer or type double",
      call. = FALSE
      )
  }

  def_value_set <- dplyr::arrange(
                     dplyr::distinct(input_data, .data[[def_ind]]),
                     .data[[def_ind]])

  if (nrow(def_value_set) != 2) {
    stop(
      "The value set of 'default_ind' must be of length two",
      call. = FALSE
      )
  }

  if (nrow(def_value_set) == 2 && any(def_value_set != c(0, 1))) {
    stop(
      "The value set of 'default_ind' must be 0 and 1",
      call. = FALSE
    )
  }
}

# validate the prepared data
validate_prepared_data <- function(data, thold) {

  nobs <- nrow(data)

  if (!(all(order(data$score) == seq_len(nobs)))) {
    stop(
      "The data frame must be sorted by the 'score' column",
      call. = FALSE
    )
  }

  if (!(length(unique(data$score)) == nobs)) {
    stop(
      "The 'score' must be unique across the rows in the data frame",
      call. = FALSE
    )
  }

  if (!(purrr::is_integer(thold)) || thold <= 0) {
    stop(
      "'threshold' must be a positive integer",
      call. = FALSE
    )
  }

  if (thold > sum(data$num_defaults)) {
    stop(
      "The 'threshold' exceeds the number of defaults in 'default_ind'",
      call. = FALSE
    )
  }
}

# validate reduce_bins()
validate_reduce_bins <- function(min_no_bins, conf_level) {

  if (!(purrr::is_integer(min_no_bins)) || !(min_no_bins >= 2)) {
    stop(
      "'min_required_bins' must be an integer larger or equal than 2",
      call. = FALSE
    )
  }

  if (!strictly_between(conf_level, 0, 1)) {
    stop(
      "'confidence_level' must be between 0 and 1",
      call. = FALSE
    )
  }
}
