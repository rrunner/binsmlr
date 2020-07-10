context("Initial binning")

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

test_that("Function prepare_data()", {
  .pd_out <- prepare_data(.df, "ord_val", "def")

  expect_equal(nrow(.pd_out), 21)
  expect_equal(length(.pd_out), 3)
  expect_equal(sum(.pd_out$num_defaults), 10)
  expect_equal(sum(.pd_out$num_obligors), 24)
  expect_equal(names(.pd_out), c("score", "num_defaults", "num_obligors"))

  rm(.pd_out)
})

test_that("Function create_initial_bins()", {
  .init_bins <- create_initial_bins(.df, 5, "ord_val", "def")

  expect_equal(length(.init_bins), 2)

  expect_equal(.init_bins[[1]]$num_obl, 13)
  expect_equal(.init_bins[[1]]$num_def, 5)
  expect_equal(.init_bins[[1]]$min_score, 100)
  expect_equal(.init_bins[[1]]$max_score, 110)
  expect_equal(.init_bins[[1]]$is_merged, FALSE)

  expect_equal(.init_bins[[2]]$num_obl, 11)
  expect_equal(.init_bins[[2]]$num_def, 5)
  expect_equal(.init_bins[[2]]$min_score, 111)
  expect_equal(.init_bins[[2]]$max_score, 120)
  expect_equal(.init_bins[[2]]$is_merged, TRUE)

  .df2 <- dplyr::mutate(.df, def = as.character(def))
  expect_error(create_initial_bins(.df2, 5, "ord_val", "def"),
               "'default_ind' must be of type integer or type double")
  .df2 <- dplyr::mutate(.df, def = as.integer(def))
  expect_identical(create_initial_bins(.df, 5, "ord_val", "def"),
                   create_initial_bins(.df2, 5, "ord_val", "def"))

  rm(.init_bins, .df2)
})

rm(.df)
