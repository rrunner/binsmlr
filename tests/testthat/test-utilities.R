context("Utilities")

test_that("Function strictly_between()", {
  expect_equal(strictly_between(0, -1, 1), TRUE)
  expect_equal(strictly_between(-1, -1, 1), FALSE)
  expect_equal(strictly_between(1, -1, 1), FALSE)
  expect_equal(strictly_between(-2, -1, 1), FALSE)
  expect_equal(strictly_between(2, -1, 1), FALSE)

  expect_equal(strictly_between(c(-2, -1, -0.5, 0, 0.5, 1, 2), -1, 1),
               c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("Function is_monotonic()", {

  .b1 <- list(
    bin(num_obl = 100, num_def = 5,  min_score = 30, max_score = 60),
    bin(num_obl = 200, num_def = 8,  min_score = 61, max_score = 80),
    bin(num_obl = 500, num_def = 10, min_score = 81, max_score = 90)
  )

  .b2 <- list(
    bin(num_obl = 100, num_def = 5,  min_score = 30, max_score = 60),
    bin(num_obl = 200, num_def = 10,  min_score = 61, max_score = 80),
    bin(num_obl = 500, num_def = 10, min_score = 81, max_score = 90)
  )

  .b3 <- list(
    bin(num_obl = 100, num_def = 5,  min_score = 30, max_score = 60),
    bin(num_obl = 200, num_def = 10,  min_score = 61, max_score = 80),
    bin(num_obl = 500, num_def = 40, min_score = 81, max_score = 90)
  )

  expect_equal(is_monotonic(.b1), TRUE)
  expect_equal(is_monotonic(.b1, decreasing = FALSE), FALSE)

  expect_equal(is_monotonic(.b2), TRUE)
  expect_equal(is_monotonic(.b2, decreasing = FALSE), FALSE)

  expect_equal(is_monotonic(.b3), FALSE)
  expect_equal(is_monotonic(.b3, decreasing = FALSE), TRUE)
  expect_output(is_monotonic(.b3, verbose = TRUE),
                "\\[1,\\]\\s+0.05\\s+0", all = FALSE)
  expect_output(is_monotonic(.b3, verbose = TRUE),
                "\\[2,\\]\\s+0.05\\s+0", all = FALSE)
  expect_output(is_monotonic(.b3, verbose = TRUE),
                "\\[3,\\]\\s+0.08\\s+1", all = FALSE)

  rm(.b1, .b2, .b3)
})
