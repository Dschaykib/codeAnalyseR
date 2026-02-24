#' Resupply and Restore the renv.lock Environment
#'
#' This function attempts to restore the project environment from the
#' `renv.lock` file. If any package installation fails, the function will
#' automatically remove the problematic package from the `renv.lock` file,
#' reinstall the package, and retry the restoration process. It continues this
#' process until the restoration is successful or all problematic packages have
#' been handled.
#'
#' @param lockfile_path A character string specifying the path to the
#'   `renv.lock` file. Defaults to `"renv.lock"`.
#'
#' @details The function uses a loop to repeatedly attempt to restore the
#' environment using `renv::restore()`. If an error occurs during the
#' restoration process, it identifies the problematic package (based on the
#' error message), removes the package from the `renv.lock` file, and reinstalls
#' it using `renv::install()`. After resolving all issues, the user is prompted
#' to restart the R session and run `renv::snapshot()` to update the lockfile.
#'
#' @note
#' - This function modifies the `renv.lock` file directly by removing problematic packages.
#' - It is recommended to create a backup of the `renv.lock` file before running this function.
#' - A message is displayed at the end, reminding the user to restart the R session and run `renv::snapshot()`.
#'
#' @import renv
#' @importFrom jsonlite fromJSON write_json
#'
#' @return The function does not return a value but updates the `renv.lock` file
#' and reinstalls packages as needed. After execution, users should restart the
#' R session and call `renv::snapshot()` to finalize the changes.
#'
#' @examples
#' \dontrun{
#' # Default usage with the "renv.lock" file in the current directory
#' resupply()
#'
#' # Specify a custom path to the renv.lock file
#' resupply(lockfile_path = "path/to/renv.lock")
#' }
#'
#' @export
resupply <- function(lockfile_path = "renv.lock",
                     use_installed = FALSE,
                     check_archive = 0) {

  # initial run settings
  try <- 0
  checking <- TRUE
  failed_pkgs <- c()
  changed_pkgs <- c()

  # add backup renv file
  file.copy(from = lockfile_path, to = paste0(lockfile_path, "_backup"), overwrite = TRUE)

  # loop over renv.lock error
  while(checking) {
    try <- try + 1
    cat(rep("#", 38), "\n")
    cat("renv restore attempt ", try, "\n")
    renv_res <- tryCatch(
      expr = {
        renv::restore(prompt = FALSE, lockfile = lockfile_path)
      },
      error = function(e) {
        return(e)
      })
    cat(rep("-", 38), "\n")

    if (!"error" %in% class(renv_res)) {
      checking <- FALSE
    } else {
      log_message <- tail(renv_res$meta$message, 1)
      if (is.null(log_message)) {
        log_message <- renv_res
        this_failed_pkg <- sub(".*'([A-Za-z0-9._]+) .*", "\\1", log_message)
      } else {
        this_failed_pkg <- sub(".*package '([^']+)'.*", "\\1", log_message)
      }



      if (length(this_failed_pkg) == 0) {
        stop("could not identify failed package")
      }

      # remove package from renv.lock file
      this_lockfile <- jsonlite::fromJSON(
        txt = lockfile_path,
        simplifyVector = FALSE
      )


      # Remove the package from the "Packages" section
      this_lockfile$Packages[[this_failed_pkg]] <- NULL

      # Write the modified lockfile back to disk
      jsonlite::write_json(
        x = this_lockfile,
        path = lockfile_path,
        auto_unbox = TRUE,
        pretty = TRUE
      )

      cat("'", this_failed_pkg, "' has been removed from the renv.lock file.\n")


      # Check if the package is already installed in a different version
      if (use_installed && this_failed_pkg %in% installed.packages()[, "Package"]) {
        installed_version <- installed.packages()[this_failed_pkg, "Version"]
        cat("Package", this_failed_pkg, "is already installed in version ",
            installed_version, ". Using this version instead.\n")

        changed_pkgs <- unique(c(changed_pkgs, this_failed_pkg))

      } else {
        # If the package is not installed, try to get newest version
        new_install <- tryCatch({
          renv::install(this_failed_pkg, prompt = FALSE)
        },
        error = function(e) {
          return(e)
        })

        if (!"error" %in% class(new_install)) {
          cat("'", this_failed_pkg, "' has been reinstalled with newest version\n")
          changed_pkgs <- unique(c(changed_pkgs, this_failed_pkg))

        } else {
          # new version failed
          # check last x archive versions
          if (is.numeric(check_archive) && check_archive > 0) {
            # get old versions
            archive_version <- get_archive_version(package = this_failed_pkg)
            archive_version <- as.character(head(x = archive_version, n = check_archive))

            old_check <- FALSE
            cat("trying ", length(archive_version), " older versions\n")
            for (i_version in archive_version) {
              old_install <- tryCatch({
                renv::install(paste0(this_failed_pkg, "@", i_version), prompt = FALSE)
              },
              error = function(e) {
                return(e)
              })

              if ("error" %in% class(old_install)) {

              } else {
                old_check <- TRUE
                break()
              }
            }

            if (old_check) {
              cat("'", this_failed_pkg,
                  "' has been reinstalled in version ",
                  i_version, "\n")
              changed_pkgs <- unique(c(changed_pkgs, this_failed_pkg))
            } else {
              cat("'", this_failed_pkg,
                  "' could not be installed\n")
              failed_pkgs <- unique(c(failed_pkgs, this_failed_pkg))
            }

          }
        }

      }
    }
  }

  if (length(failed_pkgs) > 0) {
    cat("failed the following packges:\n",
        failed_pkgs, "\n")
  }

  if (length(changed_pkgs) > 0) {
    cat("changed the following packges:\n",
        changed_pkgs, "\n")
  }

  cat("-- please RESTART R and execute renv::snapshot() --\n")
}



# from: https://github.com/mokymai/bio/blob/master/R/packages--check.R#L360

get_archive_version <- function(package) {
  # package <- "terra"
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
    as.numeric_version() %>%
    sort(decreasing = TRUE)

  return(out)
}

