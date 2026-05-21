validate_expression_matrix <- function(path) {
  read_res <- tryCatch({
    first_line <- readLines(path, n = 1L, warn = FALSE)
    sep <- if (grepl("\t", first_line)) "\t" else ","
    list(df = utils::read.csv(path, sep = sep, row.names = 1L, check.names = FALSE),
         sep = sep)
  }, error = function(e) {
    list(err = e$message)
  })
  if (!is.null(read_res$err)) {
    return(list(valid = FALSE,
                issues = paste("Cannot read file:", read_res$err),
                sample_names = character(0L),
                n_genes = 0L, n_samples = 0L, preview = ""))
  }
  df <- read_res$df

  if (nrow(df) == 0L) {
    return(list(valid = FALSE,
                issues = "File is empty or has no gene rows",
                sample_names = character(0L),
                n_genes = 0L, n_samples = 0L, preview = ""))
  }

  issues <- character(0L)
  if (ncol(df) < 2L) {
    issues <- c(issues, sprintf(
      "Only %d sample column(s) found - at least 2 samples are required",
      ncol(df)))
  }

  for (col in colnames(df)) {
    vals <- suppressWarnings(as.numeric(df[[col]]))
    bad <- which(is.na(vals) & !is.na(df[[col]]))
    if (length(bad) > 0L) {
      examples <- utils::head(df[[col]][bad], 2L)
      issues <- c(issues, sprintf(
        "Column '%s' contains non-numeric values (e.g. %s) - replace with 0 or NA",
        col, paste(examples, collapse = ", ")))
    }
  }

  dup_ids <- rownames(df)[duplicated(rownames(df))]
  if (length(dup_ids) > 0L) {
    issues <- c(issues, sprintf(
      "Duplicate gene IDs detected: %s - remove or merge duplicate rows",
      paste(utils::head(unique(dup_ids), 3L), collapse = ", ")))
  }

  preview <- tryCatch(
    paste(utils::capture.output(
      print(df[seq_len(min(3L, nrow(df))),
              seq_len(min(4L, ncol(df))),
              drop = FALSE])),
      collapse = "\n"),
    error = function(e) ""
  )

  list(valid = length(issues) == 0L,
       issues = issues,
       sample_names = colnames(df),
       n_genes = nrow(df),
       n_samples = ncol(df),
       preview = preview)
}

validate_gene_stats <- function(path) {
  read_res <- tryCatch({
    first_line <- readLines(path, n = 1L, warn = FALSE)
    sep <- if (grepl("\t", first_line)) "\t" else ","
    utils::read.csv(path, sep = sep, check.names = FALSE)
  }, error = function(e) e)
  if (inherits(read_res, "error")) {
    return(list(valid = FALSE,
                issues = paste("Cannot read file:", read_res$message),
                n_genes = 0L))
  }
  df <- read_res

  if (nrow(df) == 0L) {
    return(list(valid = FALSE, issues = "File is empty", n_genes = 0L))
  }

  required <- c("id", "logFC", "pvalue")
  found <- colnames(df)
  missing <- setdiff(required, found)
  issues <- character(0L)

  alias_hints <- list(
    id = c("ID", "gene", "gene_id", "GeneID", "Symbol", "ENSEMBL",
           "ensembl_gene_id"),
    logFC = c("log2FoldChange", "log2fc", "LFC", "logfoldchange", "Log2FC"),
    pvalue = c("p.value", "padj", "pval", "p_value", "P.Value", "adj.P.Val")
  )

  for (col in missing) {
    aliases <- alias_hints[[col]]
    candidates <- intersect(aliases, found)
    if (length(candidates) > 0L) {
      issues <- c(issues, sprintf(
        "Required column '%s' not found - rename '%s' to '%s'",
        col, candidates[[1L]], col))
    } else {
      issues <- c(issues, sprintf(
        paste("Required column '%s' not found.",
              "Columns present: %s.",
              "Common aliases to rename: %s"),
        col, paste(found, collapse = ", "),
        paste(aliases, collapse = ", ")))
    }
  }

  if (length(missing) > 0L) {
    return(list(valid = FALSE, issues = issues, n_genes = nrow(df)))
  }

  for (col in c("logFC", "pvalue")) {
    vals <- suppressWarnings(as.numeric(df[[col]]))
    bad <- which(is.na(vals) & !is.na(df[[col]]))
    if (length(bad) > 0L) {
      examples <- utils::head(df[[col]][bad], 2L)
      issues <- c(issues, sprintf(
        "Column '%s' contains non-numeric values (e.g. %s)",
        col, paste(examples, collapse = ", ")))
    }
  }

  dup_ids <- df$id[duplicated(df$id)]
  if (length(dup_ids) > 0L) {
    issues <- c(issues, sprintf(
      "Duplicate values in 'id' column: %s - remove or deduplicate",
      paste(utils::head(unique(dup_ids), 3L), collapse = ", ")))
  }

  list(valid = length(issues) == 0L,
       issues = issues,
       n_genes = nrow(df))
}

validate_experimental_design <- function(path) {
  first_line <- tryCatch(readLines(path, n = 1L, warn = FALSE),
                         error = function(e) character(0L))
  if (length(first_line) == 0L) {
    return(list(valid = FALSE,
                issues = "File is empty or could not be parsed as CSV/TSV"))
  }
  sep <- if (grepl("\t", first_line)) "\t" else ","
  df <- tryCatch(
    utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0L) {
    return(list(valid = FALSE,
                issues = "File is empty or could not be parsed as CSV/TSV"))
  }

  missing_cols <- setdiff(c("sample", "group"), colnames(df))
  if (length(missing_cols) > 0L) {
    return(list(valid = FALSE, issues = sprintf(
      "Missing required columns: %s. Found: %s",
      paste(missing_cols, collapse = ", "),
      paste(colnames(df), collapse = ", "))))
  }

  issues <- character(0L)
  samples <- trimws(df$sample)
  dupes <- unique(samples[duplicated(samples)])
  if (length(dupes) > 0L) {
    issues <- c(issues, sprintf("Duplicate sample names: %s",
                                paste(dupes, collapse = ", ")))
  }

  if ("pair" %in% colnames(df)) {
    pair_vals <- trimws(as.character(df$pair))
    non_empty <- which(nzchar(pair_vals))
    bad_rows <- non_empty[
      is.na(suppressWarnings(as.integer(pair_vals[non_empty])))
    ]
    if (length(bad_rows) > 0L) {
      issues <- c(issues, sprintf(
        "Column 'pair' must contain integers. Bad value(s) at row(s): %s",
        paste(bad_rows + 1L, collapse = ", ")))
    }
  }

  if (length(issues) > 0L) {
    return(list(valid = FALSE, issues = issues))
  }

  list(valid = TRUE,
       issues = character(0L),
       n_samples = nrow(df),
       sample_names = samples,
       groups = unique(trimws(df$group)),
       is_paired = "pair" %in% colnames(df))
}
