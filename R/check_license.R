# checks license of installed packages

check_license <- function() {

  # use renv to get current packages

  # all_packages from other function for now
  all_packages

  all_licenses <- unlist(
    lapply(
      all_packages,
      function(pkg) {
        tryCatch(
          expr = {
            packageDescription(pkg, fields="License")
          },
          error = function(e) e
        )
      }
    )
  )

  licence_dt <- data.table::data.table(
    package = all_packages,
    license = all_licenses
  )


  #table(licence_dt$license, exclude = NULL)
  licence_dt[, .N, by = license][order(N, decreasing = TRUE)]



}
