#' Find percentage of redundant code
#'
#' @param dir a folder path, which should be checked
#' @param script a character vector with script inputs
#' @param min_chunk an integer with the minimum chunk size to be tested
#' @param exclude a vector with regex patterns, that should be ignored when reading in `dir`
#' @param verbose a boolean setting the debugging prints.
#'
#' @return a data.frame with the summary of repeatable chunks
#' @export
#'
findredundance <- function(
    dir = NULL,
    script = NULL,
    min_chunk = 2,
    exclude = c("/renv/"),
    verbose = FALSE
) {

  # TODO add input checks
  # TODO adding scripts

  # get all files
  all_files <- c()

  if (!is.null(dir)) {
    all_files <- list.files(
      path = dir,
      full.names = TRUE,
      recursive = TRUE,
      pattern = ".R$", )

    # exclude files specified in exclude
    check_file_idx <- lapply(exclude, function(x) !grepl(pattern = x, all_files))
    keep_idx <- Reduce(`|`, check_file_idx)
    all_files <- all_files[keep_idx]
  }


  all_lines_per_file <- lapply(all_files, readLines)
  all_lines_raw <- unlist(all_lines_per_file)



  if (!is.null(script) ) {
    all_lines_raw <- c(all_lines_raw, script)
  }

  if (length(all_lines_raw) == 0) {
    warning("No files found or provided in 'dir' or in 'script'!")
  }

  # TODO update numbers for scripts
  if (verbose) {
    # if script is not empty add one to the count
    cat("check", length(all_files) + !is.null(script),
        "files with", length(all_lines_raw), "lines in total\n")
  }
  # cleaning of files
  all_lines <- clean_lines(all_lines_raw = all_lines_raw)

  # Set the chunk size you want to check for duplicates
  # chunk_size <- 5  # For example, a chunk size of 3 lines

  chunk_size <- min(
    # max the half can be double
    floor(length(all_lines) / 2),
    # max longest streak can be double
    max(table(cumsum(!duplicated(all_lines))))
  )
  step <- 1
  removed_lines <- 0
  lines <- all_lines

  # check min chunk size
  min_chunk <- min(min_chunk, chunk_size)

  if (verbose) {
    cat("check", length(lines), "lines,",
        "starting with largest chunk_size", chunk_size,
        "down to", min_chunk, "\n")
  }

  chunk_vector <- if (chunk_size > 0 && chunk_size >= min_chunk) {
    seq(from = chunk_size, to = min_chunk)
  } else {
    NA_integer_
  }

  # define output table
  res_df <- data.frame(
    chunk_size = chunk_vector,
    found = NA_integer_,
    max_dups = NA_integer_,
    total_dups = NA_integer_,
    rest = NA_real_
  )

  if (chunk_size == 0) {
    warning("no duplicated rows found")
  } else {

    repeat({
      ##
      #i <- 1
      chunks <- sapply(
        seq_len(length(lines) - chunk_size + 1),
        function(i) {
          # TODO check line pasting with out of range stuff
          paste(lines[i:(i + chunk_size - 1)], collapse = "")
        }
      )


      # Find duplicates in the chunks
      tab <- table(chunks)
      duplicates <- tab[tab > 1]
      num_chunks <- length(names(duplicates))

      if (num_chunks > 0) {

        # remove duplicates lines
        dups <- unique(chunks[duplicated(chunks)])
        chunk_idx <- which(chunks %in% dups)
        lines_idx <- unique(unlist(lapply(chunk_idx, function(j) {j + seq_len(chunk_size) - 1})))
        lines[lines_idx] <- ""
        lines[chunk_idx] <- paste0("# removed chunk_size ", chunk_size ,
                                   "-", seq_along(chunk_idx))
        lines <- lines[lines != ""]
        removed_lines <- removed_lines + length(lines_idx)

        max_dups <- max(tab)
        total_dups <- sum(tab[tab > 1])
        rest <- (length(all_lines)- removed_lines) / length(all_lines)
        if (verbose) {
          cat("found", num_chunks, "chunks of size", chunk_size,
              " max dubs:", max_dups,
              " total dups:", total_dups,
              " rest:", round(rest * 100, 2),
              "%\n")
        }

        row_idx <- which(res_df$chunk_size == chunk_size)
        res_df$found[row_idx] <- num_chunks
        res_df$max_dups[row_idx] <- max_dups
        res_df$total_dups[row_idx] <- total_dups
        res_df$rest[row_idx] <- rest

        # update chunk_size
        chunk_size <- min(floor(length(lines) / 2), chunk_size - step)
      } else {
        chunk_size <- chunk_size - step
      }

      if (chunk_size < min_chunk | chunk_size <= 1) {
        break()
      }

    })
  }
  # reduce to non NA rows
  out <- na.omit(res_df)

  return(out)

}
