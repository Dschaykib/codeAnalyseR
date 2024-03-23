#
all_lines <- c("a", "b", "c", "e", "a", "a", "b", "c", "f", "a", "b", "c", "1", "2", "3", "1", "2", "4", "1", "2",  "a", "b", "c", "sf",  "a", "b", "c", "1")
#findredundance(script = all_lines)
# TODO set minium chunk_size

findredundance <- function(
    dir = NULL,
    script = NULL,
    exclude = c("/renv/"),
    verbose = TRUE
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
    cat("check", length(all_files), "files with",
        length(all_lines_raw), "lines in total\n")
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
  if (verbose) {
    cat("check ", length(lines), "lines, starting with chunk_size", chunk_size, "\n")
  }

  # define output table
  res_df <- data.frame(
    chunk_size = rev(seq_len(chunk_size)),
    found = NA_integer_,
    max_dups = NA_integer_,
    rest = NA_real_
    )

  repeat({

    ##
    #i <- 1
    chunks <- sapply(1:(length(lines) - chunk_size + 1), function(i) {
      paste(lines[i:(i + chunk_size - 1)], collapse = "")
    })


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
      lines[chunk_idx] <- paste0("# removed chunk_size ", chunk_size ,"-", seq_along(chunk_idx))
      lines <- lines[lines != ""]
      removed_lines <- removed_lines + length(lines_idx)


      max_dups <- max(tab)
      rest <- (length(all_lines)- removed_lines) / length(all_lines)

      if (verbose) {
        cat("found", num_chunks, "chunks of size", chunk_size,
            " max dubs:", max_dups,
            " rest:", round(rest * 100, 2),
            "%\n")
      }

      row_idx <- which(res_df$chunk_size == chunk_size)
      res_df$found[row_idx] <- num_chunks
      res_df$max_dups[row_idx] <- max_dups
      res_df$rest[row_idx] <- rest

      # update chunk_size
      chunk_size <- min(floor(length(lines) / 2), chunk_size - step)
    } else {
      chunk_size <- chunk_size - step
    }

    if (chunk_size <= 1) {
      break()
    }

  })

  # reduce to non NA rows
  out <- na.omit(res_df)

  return(out)

}
