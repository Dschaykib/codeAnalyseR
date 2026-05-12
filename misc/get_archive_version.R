
# from: https://github.com/mokymai/bio/blob/master/R/packages--check.R#L360

get_archive_version <- function(package) {
  # package <- "ggplot2"
  # Base URL of archived packages on CRAN
  url <- "https://cran.r-project.org/src/contrib/Archive/"

  # Create the complete URL for the package
  package_url <- paste0(url, package)

  # Extract the webpage table rows containing the package versions
  rows <- try(
    suppressWarnings(readLines(package_url, warn = FALSE)),
    silent = TRUE
  )

  if (inherits(rows, "try-error")) {
    return(as.numeric_version(NULL))
  }

  # Extract the archived versions
  out <- rows %>%
    stringr::str_extract(stringr::str_glue("{package}_(.*?)[.]tar[.]gz"), 1) %>%
    na.omit() %>%
    rev() %>%
    as.character()

  return(out)
}
