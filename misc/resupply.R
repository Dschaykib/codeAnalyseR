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
resupply <- function(lockfile_path = "renv.lock") {

  # initial run settings
  try <- 0
  checking <- TRUE
  failed_pkgs <- c()

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
      this_failed_pkg <- sub(".*package '([^']+)'.*", "\\1", log_message)
      failed_pkgs <- unique(c(failed_pkgs, this_failed_pkg))

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


      # try to install the package again
      renv::install(this_failed_pkg, prompt = FALSE)

      cat("'", this_failed_pkg, "' has been reinstalled\n")
    }

  }

  if (length(failed_pkgs) > 0) {
    cat("updated the following packges:\n",
        failed_pkgs)
    cat("-- please RESTART R and execute renv::snapshot() --")
  }

}


