parse_design_csv <- function(path) {
  first_line <- readLines(path, n = 1L, warn = FALSE)
  sep <- if (grepl("\t", first_line)) "\t" else ","
  df <- utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0L) stop("experiment_design CSV is empty")
  missing_cols <- setdiff(c("sample", "group"), colnames(df))
  if (length(missing_cols) > 0L) {
    stop(sprintf(
      paste("experiment_design CSV missing required columns: %s.",
            "Found columns: %s"),
      paste(missing_cols, collapse = ", "),
      paste(colnames(df), collapse = ", ")))
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    entry <- list(sample = trimws(row$sample),
                  group  = trimws(row$group))
    if ("pair" %in% colnames(df) &&
        nzchar(trimws(as.character(row$pair %||% "")))) {
      pair_val <- suppressWarnings(as.integer(trimws(as.character(row$pair))))
      if (is.na(pair_val)) {
        stop(sprintf("Row %d: 'pair' must be an integer, got '%s'",
                     i + 1L, row$pair))
      }
      entry$pair <- pair_val
    }
    entry
  })
}
