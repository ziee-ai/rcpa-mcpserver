tool_validate_input_file <- function() {
  mcpserver::new_tool(
    name = "validate_input_file",
    description = paste(
      "Validate a gene expression matrix, gene statistics, or experimental",
      "design CSV/TSV file from a URL. Returns validation results including",
      "whether the file is valid, any issues found, sample names (for",
      "expression matrices and experimental designs), and gene/sample",
      "counts. Always call this tool first before running any analysis."),
    input_schema = mcpserver::schema(list(
      file_uri = mcpserver::property_string(
        description = paste(
          "URL to the CSV/TSV file to validate.",
          "Pass the URL exactly as provided."),
        required = TRUE),
      file_type = mcpserver::property_enum(
        values = c("expression_matrix", "gene_stats", "experimental_design"),
        description = paste(
          "Type of file to validate:",
          "'expression_matrix' (genes x samples),",
          "'gene_stats' (id/logFC/pvalue columns), or",
          "'experimental_design' (sample/group/pair columns)."),
        required = TRUE)
    )),
    annotations = list(
      readOnlyHint = TRUE,
      destructiveHint = FALSE,
      idempotentHint = TRUE,
      openWorldHint = TRUE,
      title = "Validate Input File"
    ),
    handler = validate_input_file_handler
  )
}

validate_input_file_handler <- function(args, ctx) {
  file_uri  <- trimws(args$file_uri  %||% "")
  file_type <- trimws(args$file_type %||% "")

  if (!nzchar(file_uri)) {
    return(mcp_tool_error(
      "file_uri is required - provide a download URL, not a local file path"))
  }
  if (!file_type %in% c("expression_matrix",
                         "gene_stats",
                         "experimental_design")) {
    return(mcp_tool_error(sprintf(
      "file_type must be 'expression_matrix', 'gene_stats', or 'experimental_design'. Got: '%s'",
      file_type)))
  }

  tmp_path <- tryCatch(fetch_to_tempfile(file_uri),
                       error = function(e) e)
  if (inherits(tmp_path, "error")) {
    return(mcp_tool_error(
      paste("Failed to fetch file:", conditionMessage(tmp_path))))
  }
  on.exit(unlink(tmp_path), add = TRUE)

  switch(file_type,
    "expression_matrix"   = validate_expr_response(file_type,
                                                   validate_expression_matrix(tmp_path)),
    "gene_stats"          = validate_gene_stats_response(file_type,
                                                          validate_gene_stats(tmp_path)),
    "experimental_design" = validate_design_response(file_type,
                                                      validate_experimental_design(tmp_path))
  )
}

validate_expr_response <- function(file_type, result) {
  if (!result$valid) {
    return(mcp_tool_error(
      result$issues,
      expected_format = paste(
        "CSV or TSV.",
        "Row 1 = column headers (first cell = gene ID label or empty,",
        "remaining = sample names).",
        "Rows 2+ = one gene per row (gene ID in col 1, numeric",
        "expression values in remaining columns).",
        "No duplicate gene IDs. NA values are allowed."),
      hint = "Fix the issues above, re-upload, and retry with the new URI."))
  }
  mcpserver::response_text(jsonlite::toJSON(list(
    valid        = TRUE,
    file_type    = file_type,
    n_genes      = result$n_genes,
    n_samples    = result$n_samples,
    sample_names = result$sample_names,
    preview      = result$preview
  ), auto_unbox = TRUE))
}

validate_gene_stats_response <- function(file_type, result) {
  if (!result$valid) {
    return(mcp_tool_error(
      result$issues,
      expected_format = paste(
        "CSV or TSV with exactly these column names (case-sensitive):",
        "'id' (gene identifier), 'logFC' (log fold-change, numeric),",
        "'pvalue' (p-value, numeric, between 0 and 1).",
        "One row per gene. No duplicate IDs."),
      hint = paste(
        "Rename columns to match exactly, fix any non-numeric values,",
        "re-upload, and retry.")))
  }
  mcpserver::response_text(jsonlite::toJSON(list(
    valid     = TRUE,
    file_type = file_type,
    n_genes   = result$n_genes
  ), auto_unbox = TRUE))
}

validate_design_response <- function(file_type, result) {
  if (!result$valid) {
    return(mcp_tool_error(
      result$issues,
      expected_format = paste(
        "CSV or TSV with required columns: 'sample' (sample name),",
        "'group' (experimental group).",
        "Optional column: 'pair' (integer, for paired/blocked designs).",
        "One row per sample. No duplicate sample names."),
      hint = paste(
        "Ensure column names are lowercase and exactly 'sample', 'group'",
        "(and optionally 'pair').")))
  }
  mcpserver::response_text(jsonlite::toJSON(list(
    valid        = TRUE,
    file_type    = file_type,
    n_samples    = result$n_samples,
    sample_names = result$sample_names,
    groups       = result$groups,
    is_paired    = result$is_paired
  ), auto_unbox = TRUE))
}
