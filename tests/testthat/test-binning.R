context("Binning")

test_that("Function merge_list_of_bins()", {
  .b <- list(
          bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60),
          bin(num_obl = 200, num_def = 15, min_score = 61, max_score = 80)
          )
  .m <- merge_list_of_bins(.b, c(1, 2))

  expect_equal(length(.b), 2)
  expect_equal(.b[[1]]$is_merged, FALSE)
  expect_equal(.b[[2]]$is_merged, FALSE)

  expect_equal(length(.m), 1)
  expect_equal(.m[[1]]$num_obl, 300)
  expect_equal(.m[[1]]$num_def, 20)
  expect_equal(.m[[1]]$dr, 0.06666667, tolerance = 1e-5)
  expect_equal(.m[[1]]$odds, 0.06666667 / (1 - 0.06666667), tolerance = 1e-5)
  expect_equal(.m[[1]]$min_score, 30)
  expect_equal(.m[[1]]$max_score, 80)
  expect_equal(.m[[1]]$is_merged, TRUE)

  expect_error(merge_list_of_bins(.b, c(1, 3)))
  expect_error(merge_list_of_bins(.b, c(-1, 2)))
  expect_error(merge_list_of_bins(.b, ))
  expect_error(merge_list_of_bins(.b, 0))

  rm(.b, .m)
})

test_that("Function reduce_bins()", {
  .b <- list(
          bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60),
          bin(num_obl = 200, num_def = 10, min_score = 61, max_score = 80),
          bin(num_obl = 100, num_def = 50, min_score = 81, max_score = 90),
          bin(num_obl = 200, num_def = 100, min_score = 91, max_score = 100)
          )

  expect_equal(.b[[1]]$is_merged, FALSE)
  expect_equal(.b[[2]]$is_merged, FALSE)
  expect_equal(.b[[3]]$is_merged, FALSE)
  expect_equal(.b[[4]]$is_merged, FALSE)

  .rc <- reduce_bins(.b, 2, 0.01, "chisq.test")
  .rf <- reduce_bins(.b, 2, 0.01, "fisher.test")
  expect_identical(.rc, .rf)

  expect_equal(length(.rc), 2)
  expect_equal(.rc[[1]]$num_obl, 300)
  expect_equal(.rc[[1]]$num_def, 15)
  expect_equal(.rc[[1]]$dr, 0.05)
  expect_equal(.rc[[1]]$min_score, 30)
  expect_equal(.rc[[1]]$max_score, 80)
  expect_equal(.rc[[1]]$is_merged, TRUE)
  expect_equal(.rc[[2]]$num_obl, 300)
  expect_equal(.rc[[2]]$num_def, 150)
  expect_equal(.rc[[2]]$dr, 0.5)
  expect_equal(.rc[[2]]$min_score, 81)
  expect_equal(.rc[[2]]$max_score, 100)
  expect_equal(.rc[[2]]$is_merged, TRUE)

  expect_equal(length(reduce_bins(.b, 4, 0.01, "chisq.test")), 4)
  expect_equal(length(reduce_bins(.b, 3, 0.01, "chisq.test")), 3)

  expect_error(reduce_bins(.b, 1, 0.01, "chisq.test"))
  expect_error(reduce_bins(.b, 0, 0.01, "chisq.test"))
  expect_error(reduce_bins(.b, -1, 0.01, "chisq.test"))

  rm(.b, .rc, .rf)
})

test_that("Function autobin()", {
  .a <- autobin(bin_data, 30, "score", "default", 7, 0.01)
  .b <- create_initial_bins(bin_data, 30, "score", "default")
  .b <- reduce_bins(.b, 7, 0.01)
  expect_identical(.a, .b)

  .a <- autobin(bin_data, 20, "score", "default", 2, 0.01)
  .b <- create_initial_bins(bin_data, 20, "score", "default")
  .b <- reduce_bins(.b, 2, 0.01)
  expect_identical(.a, .b)

  .a <- autobin(bin_data, 10, "score", "default", 40, 0.01)
  .b <- create_initial_bins(bin_data, 10, "score", "default")
  .b <- reduce_bins(.b, 40, 0.01)
  expect_identical(.a, .b)

  rm(.a, .b)
})
