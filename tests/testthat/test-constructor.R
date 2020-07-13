context("Constructor")

test_that("dr(): default rate", {
  expect_equal(dr(0, 10), 0)
  expect_equal(dr(5, 10), 0.5)
  expect_equal(dr(10, 10), 1)
})

test_that("odds(): odds ratio", {
  expect_equal(odds(0), 0)
  expect_equal(odds(0.5), 1)
  expect_equal(odds(1), Inf)
})

test_that("midscore(): mid score", {
  expect_equal(midscore(10, 20), 15)
  expect_equal(midscore(10, 11), 10.5)
  expect_equal(midscore(10.5, 13), 11.75)
})

test_that("bin(): helper constructor", {

  .b <- bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60)

  expect_s3_class(.b, c("bin", "list"))
  expect_equal(class(.b), c("bin", "list"))

  expect_equal(.b$num_obl, 100)
  expect_equal(.b$num_def, 5)
  expect_equal(.b$dr, 0.05)
  expect_equal(.b$odds, 0.0526, tolerance = 1e-4)
  expect_equal(.b$min_score, 30)
  expect_equal(.b$mid_score, 45)
  expect_equal(.b$max_score, 60)
  expect_equal(.b$is_merged, FALSE)
  expect_output(str(.b), "List of 8")
  expect_output(str(.b), "\\$ num_obl\\s*: int 100", all = FALSE)
  expect_output(str(.b), "\\$ num_def\\s*: int 5", all = FALSE)
  expect_output(str(.b), "\\$ dr\\s*: num 0.05", all = FALSE)
  expect_output(str(.b), "\\$ odds\\s*: num 0.0526", all = FALSE)
  expect_output(str(.b), "\\$ min_score\\s*: num 30", all = FALSE)
  expect_output(str(.b), "\\$ mid_score\\s*: num 45", all = FALSE)
  expect_output(str(.b), "\\$ max_score\\s*: num 60", all = FALSE)
  expect_output(str(.b), "\\$ is_merged\\s*: logi FALSE", all = FALSE)

  expect_error(
    bin(num_obl = -1,
        num_def = 5,
        min_score = 30,
        max_score = 60),
    "Number of obligors must be a positive integer"
    )
  expect_error(
    bin(num_obl = 100,
        num_def = -5,
        min_score = 30,
        max_score = 60),
    "Number of defaults must be a non-negative integer"
  )
  expect_error(
    bin(num_obl = 2,
        num_def = 5,
        min_score = 30,
        max_score = 60),
    "Number of obligors is less than the number of defaults"
  )
  expect_error(
    bin(num_obl = 100,
        num_def = 5,
        min_score = 61,
        max_score = 60),
    "min_score cannot exceed max_score"
  )
  expect_error(
    bin(num_obl = 100,
        num_def = 5,
        min_score = 30,
        max_score = 60,
        is_merged = NOTTRUE),
    "object 'NOTTRUE' not found"
  )

  rm(.b)
})
