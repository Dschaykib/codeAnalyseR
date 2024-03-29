#' Cleans code lines from whitespaces, comments, etc.
#'
#' @param all_lines_raw a character vector containing code lines
#' @param verbose a boolean, if TRUE logging will be printed
#'
#' @return a character vector with cleand lines of code
#' @export
#'
#' @examples
#' all_lines_raw <- c("", "# a comment", "x <- 2 + 2", "  print(x)  ")
#' clean_lines(all_lines_raw)
#' # [1] "x <- 2 + 2" "print(x)"

clean_lines <- function(all_lines_raw = c(),
                        verbose = FALSE) {

  if (!is.atomic(all_lines_raw)) {
    stop("all_lines_raw need to be a vector")
  }

  # remove trailing spaces
  all_lines_no_spaces <- trimws(all_lines_raw)
  # remove empty lines
  all_files_non_empty <- all_lines_no_spaces[all_lines_no_spaces != ""]

  if (verbose) {
  cat("- remaining ",
      round(length(all_files_non_empty) / length(all_lines_raw)*100, 4),
      "% after empty lines\n")
  }
  # remove comments
  all_files_no_comments <-
    all_files_non_empty[!startsWith(all_files_non_empty, "#")]
  if (verbose) {
  cat("- remaining ",
      round(length(all_files_no_comments) / length(all_lines_raw)*100, 4),
      "% after commented lines\n")
  }
  # remove special lines
  special_lines <- c("}", ")", "})")
  all_files_no_brackets <-
    all_files_no_comments[!all_files_no_comments %in% special_lines]
  if (verbose) {
  cat("- remaining ",
      round(length(all_files_no_brackets) / length(all_lines_raw)*100, 4),
      "% after removed brackets\n")
  }

  all_lines <- all_files_no_brackets
  if (verbose) {
  cat("final", length(all_lines), "lines (",
      round(length(all_lines) / length(all_lines_raw)*100, 4),"%)\n")
  }

  return(all_lines)
}
