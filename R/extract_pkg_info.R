#' Extract package information
#'
#' @description
#' A short wrapper function to get a packages dependencies and it's source.
#' The input is one list element from `renv::lockfile_read()`.
#' It should contain:
#'
#' * Package: the package name
#' * Source: where the packages was sourced, this needs the additional info from the repository entry
#' * Repository: additional info about the package source
#' * Requirements: the dependencies of the package
#'
#'
#' @param pkg a list with one package information from a renv lockfile.
#'
#' @return a data.frame with the package, its dependencies and the source
#'
#' @examples
#' pkg <- list(Package ="DBI", Source = "Repository", Repository = "CRAN",
#'             Requirements = c("R", "methods"))
#' extract_pkg_info(pkg = pkg)
#'
#' #   pkg     dep source
#' # 1 DBI       R   CRAN
#' # 2 DBI methods   CRAN
#'
extract_pkg_info <- function(pkg) {

  #pkg <- renv_pkgs$Packages[[1]]
  deps <- pkg$Requirements
  this_source <- ifelse(pkg$Source == "Repository",
                        pkg$Repository,
                        pkg$Source
  )

  if (length(deps) == 0) {
    out <- data.frame()
  } else {
    out <- data.frame(
      pkg = pkg$Package,
      dep = deps,
      source = this_source,
      stringsAsFactors = FALSE
    )
  }
  return(out)
}
