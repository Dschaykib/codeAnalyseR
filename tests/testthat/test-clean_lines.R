testthat::context("check clean_lines.R")


# error -------------------------------------------------------------------

testthat::test_that("white spaces and comments are removed", {

  lines <- c("", "# a comment", "x <- 2 + 2", "  print(x)  ")
  res <- c("x <- 2 + 2", "print(x)")

  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

})

testthat::test_that("closing brakets are removed", {

  lines <- c(")", "}", ")}", "})")
  res <- c(")}")

  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

})

testthat::test_that("input is a list", {

  lines <- c(")", "}", ")}", "})")
  res <- c(")}")

  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

})


testthat::test_that("non-character inputs work", {

  lines <- c(1:3)
  res <- c("1", "2", "3")
  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

  lines <- c(TRUE, FALSE)
  res <- c("TRUE", "FALSE")
  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

  lines <- c(NA)
  res <- c(NA_character_)
  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

  lines <- c(NULL)
  res <- character(0)
  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

  lines <- c(NULL)
  res <- character(0)
  testthat::expect_equal(clean_lines(all_lines_raw = lines), res)

})

