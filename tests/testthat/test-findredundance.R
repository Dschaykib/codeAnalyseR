testthat::context("check findredundance.R")

testthat::test_that("check chunk size", {

  test_script <- c(
    "1", "2", "3", "4", "5",
    "c", "b",
    "1", "2", "3", "4", "5",
    "a1",
    "a", "b", "1",
    "a2",
    "a", "b", "2",
    "a3",
    "a", "b", "3",
    "a4",
    "a", "b", "4"
  )


  tmp <- findredundance(script = test_script, verbose = TRUE)

  # check table values of most repeats and number of repeats
  testthat::expect_equal(max(tmp$chunk_size), 5)
  testthat::expect_equal(min(tmp$chunk_size), 2)
  testthat::expect_equal(max(tmp$max_dups), 4)

  # checks if the leftover percentage is expected
  testthat::expect_equal(tmp$rest, c((28 - 10) / 28, (28 - 18) / 28))


})

testthat::test_that("no repeats work", {
  test_script <- c("no repeats")
  testthat::expect_warning(findredundance(script = test_script))
})

testthat::test_that("only repeats work", {
  test_script <- rep(c("1"), 10)

  debugonce(findredundance)
  tmp <- findredundance(script = test_script, verbose = TRUE)

  # TODO fix output
  testthat::expect(nrow(tmp), 1)
  testthat::expect_equal(tmp$chunk_size, 5)
  testthat::expect_equal(min(tmp$chunk_size), 2)
  testthat::expect_equal(max(tmp$max_dups), 4)

})
