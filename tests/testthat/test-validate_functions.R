context("Validations")

set_bin_values <- function(var = NULL, value = NULL) {
  bin <- bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60)

  if (exists(x = var, where = bin)) {
    bin[[var]] <- value
  } else {
    warning("Variable ", var, " does not exist in bin object")
  }

  bin
}

test_that("validate_new_bin(): Validation of num_obl", {
  expect_error(validate_new_bin(set_bin_values("num_obl", -1)))
  expect_error(validate_new_bin(set_bin_values("num_obl", 0)))
})

test_that("validate_new_bin(): Validation of num_def", {
  expect_error(validate_new_bin(set_bin_values("num_def", -1)))
})

test_that("validate_new_bin(): Validation of num_obl/num_def", {
  expect_error(validate_new_bin(set_bin_values("num_obl", 4)))
})

test_that("validate_new_bin(): Validation of dr", {
  expect_error(validate_new_bin(set_bin_values("dr", -0.1)))
  expect_error(validate_new_bin(set_bin_values("dr",  1.1)))
})

test_that("validate_new_bin(): Validation of odds", {
  expect_error(validate_new_bin(set_bin_values("odds", -0.1)))
})

test_that("validate_new_bin(): min_score > max_score", {
  expect_error(validate_new_bin(set_bin_values("min_score", 61)))
})

test_that("validate_new_bin(): mid_score within range", {
  expect_error(validate_new_bin(set_bin_values("mid_score", 29)))
  expect_error(validate_new_bin(set_bin_values("mid_score", 61)))
})

test_that("validate_new_bin(): that is_merged is logical", {
  expect_error(validate_new_bin(set_bin_values("is_merged", 0)))
  expect_error(validate_new_bin(set_bin_values("is_merged", 1)))
  expect_error(validate_new_bin(set_bin_values("is_merged", true)))
  expect_error(validate_new_bin(set_bin_values("is_merged", false)))

  expect_equal(
    validate_new_bin(
      set_bin_values("is_merged", T))["is_merged"][[1]], TRUE
    )
  expect_equal(
    validate_new_bin(
      set_bin_values("is_merged", F))["is_merged"][[1]], FALSE
    )
})

rm(set_bin_values)


test_that("validate_merge_list_of_bins(): ", {
  expect_error(validate_merge_list_of_bins(c(-1, 1, 2)))
  expect_error(validate_merge_list_of_bins(c(1)))
  expect_error(validate_merge_list_of_bins(c(1, 3, 2)))
  expect_error(validate_merge_list_of_bins(c(1, 2, 4)))
})


.df <- dplyr::tribble(~ord_val, ~def,
                      100,    1,
                      110,    1,
                      109,    0,
                      105,    0,
                      103,    1,
                      107,    0,
                      106,    0,
                      108,    0,
                      102,    1,
                      107,    0,
                      104,    0,
                      101,    1,
                      110,    0,

                      120,    0,
                      119,    0,
                      115,    1,
                      113,    1,
                      117,    0,
                      116,    0,
                      118,    0,
                      112,    1,
                      117,    0,
                      114,    1,
                      111,    1)

test_that("validate_input_data(): default indicator", {

  .df2 <- dplyr::mutate(.df, def = as.character(def))
  expect_error(create_initial_bins(.df2, 5, "ord_val", "def"),
               "'default_ind' must be of type integer or type double")

  .df2 <- dplyr::add_row(.df, dplyr::tibble(ord_val = 100, def = 3))
  expect_error(create_initial_bins(.df2, 5, "ord_val", "def"),
               "value set of 'default_ind' must be of length two")

  .df2 <- dplyr::mutate(.df, def = def + 1)
  expect_error(create_initial_bins(.df2, 5, "ord_val", "def"),
               "value set of 'default_ind' must be 0 and 1")

  .df2 <- dplyr::mutate(.df, def = as.integer(def))
  expect_identical(create_initial_bins(.df, 5, "ord_val", "def"),
                   create_initial_bins(.df2, 5, "ord_val", "def"))

  rm(.df2)
})


test_that("validate_prepared_data(): sorting, uniqueness and thresholds", {
  .df2 <- prepare_data(.df, "ord_val", "def")
  .df2 <- dplyr::add_row(dplyr::tibble(score = 600, num_defaults = 0,
                                       num_obligors = 1), .df2)
  expect_error(
    validate_prepared_data(.df2, 5L),
    "data frame must be sorted by the 'score' column"
    )

  .df2 <- prepare_data(.df, "ord_val", "def")
  .df2 <- dplyr::add_row(dplyr::tibble(score = 100, num_defaults = 0,
                                       num_obligors = 1), .df2)
  expect_error(
    validate_prepared_data(.df2, 5L),
    "'score' must be unique across the rows in the data frame"
  )

  .df2 <- prepare_data(.df, "ord_val", "def")

  expect_error(
    validate_prepared_data(.df2, 5),
    "'threshold' must be a positive integer"
  )

  expect_error(
    validate_prepared_data(.df2, 0L),
    "'threshold' must be a positive integer"
  )

  expect_error(
    validate_prepared_data(.df2, -1L),
    "'threshold' must be a positive integer"
  )

  expect_error(
    validate_prepared_data(.df2, 11L),
    "'threshold' exceeds the number of defaults in 'default_ind'"
  )

  rm(.df2)
})


test_that("validate_reduce_bins(): argument validations", {
  .b <- list(
    bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60),
    bin(num_obl = 200, num_def = 10, min_score = 61, max_score = 80),
    bin(num_obl = 100, num_def = 50, min_score = 81, max_score = 90),
    bin(num_obl = 200, num_def = 100, min_score = 91, max_score = 100)
  )

  expect_error(
    reduce_bins(.b, 1, 0.01),
    "'min_required_bins' must be an integer larger or equal than 2"
    )

  expect_error(
    reduce_bins(.b, 0, 0.01),
    "'min_required_bins' must be an integer larger or equal than 2"
    )

  expect_error(
    reduce_bins(.b, -1, 0.01),
    "'min_required_bins' must be an integer larger or equal than 2"
    )

  expect_error(
    reduce_bins(.b, 2, 0),
    "'confidence_level' must be between 0 and 1"
  )

  expect_error(
    reduce_bins(.b, 2, 1),
    "'confidence_level' must be between 0 and 1"
  )

  rm(.b)
})

rm(.df)
