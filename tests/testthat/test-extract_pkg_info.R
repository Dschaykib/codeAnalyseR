testthat::context("check extract_pkg_info.R")


# error -------------------------------------------------------------------

testthat::test_that("examples works", {

  pkg <- list(
    Package = "DBI",
              Source = "Repository",
              Repository = "CRAN",
              Requirements = c("R", "methods"))

  res <- data.frame(
    pkg = c("DBI", "DBI"),
    dep = c("R", "methods"),
    source = c("CRAN", "CRAN")
  )

  testthat::expect_equal(extract_pkg_info(pkg = pkg), res)

})


# test scripts with functions ---------------------------------------------

testthat::test_that("renv output has not changed", {

  tmp_renv <- renv::lockfile_read("../../renv.lock")
  # tmp_renv <- renv::lockfile_read("renv.lock")

  testthat::expect_contains(
    object = names(tmp_renv),
    expected = c("Packages"))


  testthat::expect_contains(
    object = names(tmp_renv$Packages[[1]]),
    expected = c("Source", "Repository", "Package", "Requirements"))

})
