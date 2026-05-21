.DESIGN_CSV_FORMAT <- paste(
  "CSV with columns: 'sample' (must match expression matrix column",
  "headers), 'group' (e.g. 'Control', 'Treatment').",
  "Optional third column 'pair' (integer) for paired/blocked designs.")

tool_run_de_analysis <- function() {
  mcpserver::new_tool(
    name = "run_de_analysis",
    description = paste(
      "Run differential expression analysis on a gene expression matrix.",
      "Requires an expression matrix CSV and an experiment design CSV.",
      "Returns a CSV of DE results (logFC, pvalue, pFDR per gene).",
      "Only include optional parameters when the user has explicitly",
      "specified a value."),
    input_schema = mcpserver::schema(list(
      expression_uri = mcpserver::property_string(
        description = paste(
          "URL to the expression matrix CSV.",
          "Pass it exactly as provided."),
        required = TRUE),
      experiment_design_uri = mcpserver::property_string(
        description = paste("URL to the experiment design CSV.",
                            .DESIGN_CSV_FORMAT),
        required = TRUE),
      method = mcpserver::property_enum(
        values = c("limma", "DESeq2", "edgeR"),
        description = paste(
          "DE method: 'limma' (microarray/normalized RNA-seq),",
          "'DESeq2' (raw counts), 'edgeR' (raw counts).",
          "Only include if the user has explicitly specified a method;",
          "otherwise omit entirely.")),
      contrast = mcpserver::property_string(
        description = paste(
          "Contrast string, e.g. 'Treatment - Control'.",
          "Inferred automatically when exactly 2 groups.")),
      gene_id_type = mcpserver::property_string(
        description = "Type of gene identifiers. Only 'entrez' is supported.")
    )),
    annotations = list(
      readOnlyHint = FALSE,
      destructiveHint = FALSE,
      idempotentHint = FALSE,
      openWorldHint = TRUE,
      title = "Run DE Analysis"
    ),
    handler = run_de_analysis_handler
  )
}

run_de_analysis_handler <- function(args, ctx) {
  prep <- de_analysis_prepare(args)
  if (!is.null(prep$error)) return(prep$error)
  job <- run_job_sync(prep$script_path, prep$job_name)
  safe_unlink_all(prep$tmp_files)
  de_build_response(job, prep)
}

de_analysis_prepare <- function(args) {
  expression_uri <- trimws(args$expression_uri %||% "")
  experiment_design_uri <- trimws(args$experiment_design_uri %||% "")
  method <- trimws(args$method %||% "")
  contrast <- trimws(args$contrast %||% "")
  gene_id_type <- trimws(args$gene_id_type %||% "entrez")

  tmp_expr <- NULL
  tmp_design_csv <- NULL
  tmp_design_json <- NULL
  fail <- function(resp) {
    safe_unlink_all(list(tmp_expr, tmp_design_csv, tmp_design_json))
    list(error = resp)
  }

  if (!identical(gene_id_type, "entrez")) {
    return(fail(mcp_tool_error(
      "Only Entrez gene IDs are supported at this time.",
      hint = "Convert gene IDs to Entrez IDs before running DE analysis.")))
  }
  if (!nzchar(expression_uri)) {
    return(fail(mcp_tool_error(paste(
      "expression_uri is required - provide a download URL,",
      "not a local file path"))))
  }

  tmp_expr <- tryCatch(fetch_to_tempfile(expression_uri),
                       error = function(e) e)
  if (inherits(tmp_expr, "error")) {
    return(fail(mcp_tool_error(paste("Failed to fetch expression_uri:",
                                     conditionMessage(tmp_expr)))))
  }

  vr <- validate_expression_matrix(tmp_expr)
  if (!vr$valid) {
    return(fail(mcp_tool_error(
      vr$issues,
      expected_format = paste(
        "CSV or TSV. Row 1 = column headers (first cell = gene ID label",
        "or empty, remaining = sample names).",
        "Rows 2+ = one gene per row (gene ID in col 1, numeric expression",
        "values in remaining columns).",
        "No duplicate gene IDs. NA values are allowed."),
      hint = "Fix via code execution, re-upload, and retry with the corrected file URI.")))
  }

  if (!nzchar(experiment_design_uri)) {
    return(fail(mcp_tool_error(
      "experiment_design_uri is required",
      expected_format = .DESIGN_CSV_FORMAT,
      sample_names_found = paste(vr$sample_names, collapse = ", "),
      hint = sprintf(paste(
        "Ask the user which of these %d samples belong to each",
        "experimental group, create a design CSV, upload it, and",
        "pass the URI."),
        length(vr$sample_names)))))
  }

  tmp_design_csv <- tryCatch(fetch_to_tempfile(experiment_design_uri),
                             error = function(e) e)
  if (inherits(tmp_design_csv, "error")) {
    return(fail(mcp_tool_error(paste("Failed to fetch experiment_design_uri:",
                                     conditionMessage(tmp_design_csv)))))
  }

  design <- tryCatch(parse_design_csv(tmp_design_csv),
                     error = function(e) e)
  if (inherits(design, "error")) {
    return(fail(mcp_tool_error(conditionMessage(design),
                                expected_format = .DESIGN_CSV_FORMAT)))
  }

  if (!nzchar(method)) method <- "limma"
  if (!method %in% c("limma", "DESeq2", "edgeR")) {
    return(fail(mcp_tool_error(sprintf(
      "method must be 'limma', 'DESeq2', or 'edgeR'. Got: '%s'", method))))
  }

  groups <- unique(vapply(design, `[[`, character(1L), "group"))
  if (!nzchar(contrast)) {
    if (length(groups) == 2L) {
      contrast <- paste0(groups[[2L]], " - ", groups[[1L]])
    } else {
      return(fail(mcp_tool_error(
        sprintf(paste("contrast is required when more than 2 groups are present.",
                      "Groups found: %s"),
                paste(groups, collapse = ", ")),
        expected_format = "A contrast string, e.g. 'Treated - Control'")))
    }
  }

  contrast_groups <- trimws(strsplit(contrast, "-", fixed = TRUE)[[1L]])
  unknown_in_contrast <- setdiff(contrast_groups, groups)
  if (length(unknown_in_contrast) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "contrast references unknown group(s): %s. Known groups: %s",
      paste(unknown_in_contrast, collapse = ", "),
      paste(groups, collapse = ", ")))))
  }

  pairs <- lapply(design, function(d) d$pair)
  has_pairs <- !any(vapply(pairs, is.null, logical(1L)))

  safe_contrast <- gsub("[^a-z0-9]+", "_", tolower(contrast))
  base_name <- paste0("de_", safe_contrast, "_", method)
  run <- make_run_dir()
  csv_filename <- paste0(base_name, ".csv")
  rds_path <- file.path(run$dir, paste0(base_name, ".rds"))
  csv_path <- file.path(run$dir, csv_filename)
  csv_url <- paste0(base_url(), "/results/", run$run_id, "/", csv_filename)

  tmp_design_json <- tempfile(fileext = ".json")
  jsonlite::write_json(design, tmp_design_json, auto_unbox = TRUE)

  job <- make_job_script(run$dir, base_name, "de_analysis", list(
    expr_path        = tmp_expr,
    design_json_path = tmp_design_json,
    method           = method,
    contrast         = contrast,
    has_pairs        = has_pairs,
    pairs            = if (has_pairs) unlist(pairs) else NULL,
    rds_path         = rds_path,
    csv_path         = csv_path
  ))

  list(
    error = NULL,
    script_path = job$script_path,
    job_name = base_name,
    csv_path = csv_path,
    csv_url = csv_url,
    csv_filename = csv_filename,
    method = method,
    contrast = contrast,
    run_id = run$run_id,
    tmp_files = list(tmp_expr, tmp_design_csv, tmp_design_json)
  )
}

de_build_response <- function(job_result, prep) {
  if (!job_result$success) {
    return(mcp_tool_error(paste("DE analysis failed:", job_result$stderr)))
  }
  de_csv <- tryCatch(utils::read.csv(prep$csv_path,
                                      stringsAsFactors = FALSE),
                     error = function(e) NULL)
  n_genes <- if (!is.null(de_csv)) nrow(de_csv) else NA_integer_
  n_sig <- if (!is.null(de_csv) && "pFDR" %in% colnames(de_csv)) {
    sum(!is.na(de_csv$pFDR) & de_csv$pFDR < 0.05, na.rm = TRUE)
  } else {
    NA_integer_
  }

  metadata_json <- jsonlite::toJSON(list(
    result_type   = "de_result",
    method        = prep$method,
    contrast      = prep$contrast,
    n_genes       = n_genes,
    n_significant = n_sig,
    run_id        = prep$run_id
  ), auto_unbox = TRUE)

  list(
    content = list(
      mcpserver::response_text(metadata_json),
      mcpserver::response_resource_link(
        uri = prep$csv_url,
        name = prep$csv_filename,
        mime_type = "text/csv")
    ),
    isError = FALSE
  )
}
