context("Methods")

.b1 <- list(
        bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60)
        )

.b2 <- list(
         bin(num_obl = 100, num_def = 5, min_score = 30, max_score = 60),
         bin(num_obl = 200, num_def = 10, min_score = 61, max_score = 90)
         )

.b3 <- merge_list_of_bins(.b2, 1:2)

test_that("print.bin(): print single bin from 'list of bins' of length one", {
  expect_output(print(.b1[[1]]), "Bin\\s+\\n---", all = FALSE)
  expect_output(print(.b1[[1]]), "\\n\\nObligors:\\s*100", all = FALSE)
  expect_output(print(.b1[[1]]), "\\nDefaults:\\s*5", all = FALSE)
  expect_output(print(.b1[[1]]), "\\n\\nDefault\\s+rate:\\s*0.05", all = FALSE)
  expect_output(print(.b1[[1]]), "\\nOdds\\s+ratio:\\s*0.0526", all = FALSE)
  expect_output(print(.b1[[1]]), "\\n\\nMin\\s+score:\\s*30", all = FALSE)
  expect_output(print(.b1[[1]]), "\\nMid\\s+score:\\s*45", all = FALSE)
  expect_output(print(.b1[[1]]), "\\nMax\\s+score:\\s*60", all = FALSE)
})

test_that("print.bin(): print first bin from 'list of bins' of length two", {
  expect_output(print(.b2[[1]]), "Bin\\s+\\n---", all = FALSE)
  expect_output(print(.b2[[1]]), "\\n\\nObligors:\\s*100", all = FALSE)
  expect_output(print(.b2[[1]]), "\\nDefaults:\\s*5", all = FALSE)
  expect_output(print(.b2[[1]]), "\\n\\nDefault\\s+rate:\\s*0.05", all = FALSE)
  expect_output(print(.b2[[1]]), "\\nOdds\\s+ratio:\\s*0.0526", all = FALSE)
  expect_output(print(.b2[[1]]), "\\n\\nMin\\s+score:\\s*30", all = FALSE)
  expect_output(print(.b2[[1]]), "\\nMid\\s+score:\\s*45", all = FALSE)
  expect_output(print(.b2[[1]]), "\\nMax\\s+score:\\s*60", all = FALSE)
})

test_that("print.bin(): print second bin from 'list of bins' of length two", {
  expect_output(print(.b2[[2]]), "Bin\\s+\\n---", all = FALSE)
  expect_output(print(.b2[[2]]), "\\n\\nObligors:\\s*200", all = FALSE)
  expect_output(print(.b2[[2]]), "\\nDefaults:\\s*10", all = FALSE)
  expect_output(print(.b2[[2]]), "\\n\\nDefault\\s+rate:\\s*0.05", all = FALSE)
  expect_output(print(.b2[[2]]), "\\nOdds\\s+ratio:\\s*0.0526", all = FALSE)
  expect_output(print(.b2[[2]]), "\\n\\nMin\\s+score:\\s*61", all = FALSE)
  expect_output(print(.b2[[2]]), "\\nMid\\s+score:\\s*75.5", all = FALSE)
  expect_output(print(.b2[[2]]), "\\nMax\\s+score:\\s*90", all = FALSE)
})

test_that("print.bin(): print merged bin", {
  expect_output(print(.b3[[1]]), "Bin\\s+\\(merged\\)\\s+\\n---", all = FALSE)
  expect_output(print(.b3[[1]]), "\\n\\nObligors:\\s*300", all = FALSE)
  expect_output(print(.b3[[1]]), "\\nDefaults:\\s*15", all = FALSE)
  expect_output(print(.b3[[1]]), "\\n\\nDefault\\s+rate:\\s*0.05", all = FALSE)
  expect_output(print(.b3[[1]]), "\\nOdds\\s+ratio:\\s*0.0526", all = FALSE)
  expect_output(print(.b3[[1]]), "\\n\\nMin\\s+score:\\s*30", all = FALSE)
  expect_output(print(.b3[[1]]), "\\nMid\\s+score:\\s*60", all = FALSE)
  expect_output(print(.b3[[1]]), "\\nMax\\s+score:\\s*90", all = FALSE)
})

rm(.b1, .b2, .b3)
