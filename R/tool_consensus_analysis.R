.PATHWAY_METHODS_CONSENSUS <- c("spia", "cepaORA", "cepaGSA")

tool_run_consensus_analysis <- function() {
  mcpserver::new_tool(
    name = "run_consensus_analysis",
    description = paste(
      "Run consensus pathway analysis by combining results from 2 or more",
      "PA methods on a single dataset.",
      "Accepted inputs:",
      "(a) gene_stats_uri - CSV with id/logFC/pvalue;",
      "(b) expression_uri + experiment_design_uri - raw expression matrix",
      "(runs DE internally).",
      "Supports KEGG and GO databases.",
      "An elicitation-capable client will be prompted interactively for",
      "DE method (if expression input), database, PA methods (>= 2),",
      "organism, GO namespace, and consensus parameters when not provided."),
    input_schema = mcpserver::schema(list(
      gene_stats_uri = mcpserver::property_string(
        description = "URL to a gene stats CSV (id, logFC, pvalue)."),
      expression_uri = mcpserver::property_string(
        description = "URL to expression matrix CSV."),
      experiment_design_uri = mcpserver::property_string(
        description = paste("URL to experiment design CSV.",
                            .DESIGN_CSV_FORMAT)),
      contrast = mcpserver::property_string(
        description = "Contrast string, e.g. 'Treatment - Control'."),
      de_method = mcpserver::property_enum(
        values = c("limma", "DESeq2", "edgeR"),
        description = "DE method when using expression input."),
      database = mcpserver::property_enum(
        values = c("KEGG", "GO"),
        description = "Pathway database. Omit to trigger interactive selection."),
      pa_methods = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Two or more PA methods to combine."),
      org = mcpserver::property_string(
        description = "KEGG organism code, e.g. hsa, mmu, rno."),
      namespace = mcpserver::property_enum(
        values = c("biological_process",
                   "molecular_function",
                   "cellular_component"),
        description = "GO namespace."),
      method = mcpserver::property_enum(
        values = c("weightedZMean", "RRA"),
        description = "Consensus aggregation method."),
      use_fdr = mcpserver::property_boolean(
        description = "Use FDR-adjusted p-values for consensus ranking."),
      weights = mcpserver::property_array(
        items = mcpserver::property_number(),
        description = "Optional numeric weights per PA method."),
      rank_by = mcpserver::property_enum(
        values = c("normalizedScore", "pFDR", "both"),
        description = "Ranking criterion for RRA.")
    )),
    annotations = list(
      readOnlyHint = FALSE,
      destructiveHint = FALSE,
      idempotentHint = FALSE,
      openWorldHint = TRUE,
      title = "Run Consensus Analysis"
    ),
    bidirectional = TRUE,
    handler = run_consensus_handler
  )
}

run_consensus_handler <- function(args, ctx) {
  filled <- elicit_consensus_args(args, ctx)
  if (is_tool_error(filled)) return(filled)
  prep <- consensus_prepare(filled,
                            filled$database, filled$pa_methods,
                            filled$org, filled$namespace,
                            filled$de_method, filled$method,
                            filled$use_fdr, filled$weights,
                            filled$rank_by)
  if (!is.null(prep$error)) return(prep$error)
  job <- run_job_sync(prep$script_path, prep$job_name)
  safe_unlink_all(prep$tmp_files)
  consensus_build_response(job, prep)
}

elicit_consensus_args <- function(args, ctx) {
  database <- trimws(args$database %||% "")
  pa_methods <- if (is.character(args$pa_methods)) {
    as.list(args$pa_methods)
  } else {
    args$pa_methods %||% list()
  }
  org <- trimws(args$org %||% "")
  namespace <- trimws(args$namespace %||% "")
  de_method <- trimws(args$de_method %||% "")
  method <- trimws(args$method %||% "")
  use_fdr <- args$use_fdr %||% TRUE
  rank_by <- trimws(args$rank_by %||% "")

  caps <- ctx$client_capabilities %||% list()
  can_elicit <- !is.null(caps$elicitation)

  has_expression <- nzchar(trimws(args$expression_uri %||% ""))

  if (has_expression && !nzchar(de_method) && can_elicit) {
    schema <- list(
      type = "object",
      required = I("method"),
      properties = list(
        method = list(
          type = "string", title = "DE Method",
          enum = I(c("limma", "DESeq2", "edgeR")),
          default = "limma"
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = paste("Which differential expression method",
                      "would you like to use?"),
      requested_schema = schema), error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      de_method <- res$content$method %||% "limma"
    }
  }
  if (!nzchar(de_method)) de_method <- "limma"

  if (!nzchar(database) || length(pa_methods) < 2L) {
    if (!can_elicit) {
      return(mcp_tool_error(
        "database and at least 2 pa_methods are required."))
    }
    schema <- list(
      type = "object",
      required = I(c("database", "pa_methods")),
      properties = list(
        database = list(
          type = "string", title = "Pathway Database",
          enum = I(c("KEGG", "GO")),
          default = "KEGG"
        ),
        pa_methods = list(
          type = "array",
          title = "Analysis Methods (select 2 or more)",
          minItems = 2L,
          items = list(enum = I(c("ora", "fgsea", "ks", "wilcox",
                                  "spia", "cepaORA", "cepaGSA"))),
          default = I(c("ora", "fgsea"))
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = paste(
        "Select pathway database and at least 2 analysis methods to",
        "combine with consensus analysis."),
      requested_schema = schema), error = function(e) e)
    if (inherits(res, "error")) {
      return(mcp_tool_error(paste("Elicitation failed:",
                                   conditionMessage(res))))
    }
    if (!identical(res$action %||% "accept", "accept")) {
      return(mcp_tool_error("User declined to choose database/methods."))
    }
    database <- res$content$database %||% "KEGG"
    pa_methods <- as.list(res$content$pa_methods %||% list("ora", "fgsea"))
  }

  if (identical(database, "GO") && !nzchar(namespace) && can_elicit) {
    schema <- list(
      type = "object", required = I("namespace"),
      properties = list(
        namespace = list(
          type = "string", title = "GO Namespace",
          enum = I(c("biological_process", "molecular_function",
                     "cellular_component")),
          default = "biological_process"
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = "Which GO namespace?",
      requested_schema = schema), error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      namespace <- res$content$namespace %||% "biological_process"
    }
  }
  if (!nzchar(namespace)) namespace <- "biological_process"

  if (!nzchar(org) && can_elicit) {
    schema <- list(
      type = "object", required = I("org"),
      properties = list(
        org = list(type = "string",
                   title = "Organism (KEGG code)",
                   default = "hsa")
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = "Which organism (KEGG code)?",
      requested_schema = schema), error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      org <- trimws(res$content$org %||% "hsa")
    }
  }

  if (!nzchar(method) && can_elicit) {
    schema <- list(
      type = "object", required = I("method"),
      properties = list(
        method = list(
          type = "string", title = "Consensus Method",
          enum = I(c("weightedZMean", "RRA")),
          default = "weightedZMean"
        ),
        use_fdr = list(type = "boolean",
                       title = "Use FDR-adjusted p-values",
                       default = TRUE),
        rank_by = list(
          type = "string", title = "Ranking criterion (RRA only)",
          enum = I(c("normalizedScore", "pFDR", "both")),
          default = "normalizedScore"
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = "Configure consensus analysis options.",
      requested_schema = schema), error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      method <- res$content$method %||% "weightedZMean"
      use_fdr <- res$content$use_fdr %||% TRUE
      rank_by <- res$content$rank_by %||% "normalizedScore"
    }
  }
  if (!nzchar(method)) method <- "weightedZMean"
  if (!nzchar(rank_by)) rank_by <- "normalizedScore"

  args$database <- database
  args$pa_methods <- pa_methods
  args$org <- org
  args$namespace <- namespace
  args$de_method <- de_method
  args$method <- method
  args$use_fdr <- use_fdr
  args$rank_by <- rank_by
  args
}

consensus_prepare <- function(args, database, pa_methods, org, namespace,
                              de_method, method, use_fdr, weights, rank_by) {
  gene_stats_uri <- trimws(args$gene_stats_uri %||% "")
  expression_uri <- trimws(args$expression_uri %||% "")
  design_uri <- trimws(args$experiment_design_uri %||% "")
  contrast <- trimws(args$contrast %||% "")

  tmp_gene_stats <- NULL
  tmp_expr <- NULL
  tmp_design_csv <- NULL
  tmp_design_json <- NULL

  fail <- function(resp) {
    safe_unlink_all(list(tmp_gene_stats, tmp_expr,
                         tmp_design_csv, tmp_design_json))
    list(error = resp)
  }

  if (!validate_org(org)) {
    return(fail(mcp_tool_error(sprintf(
      paste("Invalid KEGG organism code: '%s'.",
            "Must be 3-5 lowercase letters (e.g. hsa, mmu, rno)."),
      org))))
  }
  if (!database %in% c("KEGG", "GO")) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid database: '%s'. Must be 'KEGG' or 'GO'.", database))))
  }
  pa_methods_vec <- unlist(pa_methods)
  valid_methods <- c("ora", "fgsea", "gsa", "ks", "wilcox",
                     "spia", "cepaORA", "cepaGSA")
  invalid <- setdiff(pa_methods_vec, valid_methods)
  if (length(invalid) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid PA method(s): %s. Valid: %s",
      paste(invalid, collapse = ", "),
      paste(valid_methods, collapse = ", ")))))
  }
  if (length(pa_methods_vec) < 2L) {
    return(fail(mcp_tool_error(
      "At least 2 PA methods are required for consensus analysis.")))
  }
  if (!method %in% c("weightedZMean", "RRA")) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid consensus method: '%s'. Must be 'weightedZMean' or 'RRA'.",
      method))))
  }
  if (!namespace %in% c("biological_process",
                         "molecular_function",
                         "cellular_component")) {
    namespace <- "biological_process"
  }

  if (identical(database, "GO")) {
    kegg_only_used <- intersect(pa_methods_vec, .PATHWAY_METHODS_CONSENSUS)
    if (length(kegg_only_used) > 0L) {
      return(fail(mcp_tool_error(sprintf(
        paste("Methods %s are KEGG-only and cannot be used with GO.",
              "Use KEGG database or choose fgsea/ora/ks/wilcox."),
        paste(kegg_only_used, collapse = ", ")))))
    }
  }

  if (!is.null(weights) && length(weights) > 0L &&
      length(weights) != length(pa_methods_vec)) {
    return(fail(mcp_tool_error(sprintf(
      "weights length (%d) must match pa_methods length (%d)",
      length(weights), length(pa_methods_vec)))))
  }

  has_gene_stats <- nzchar(gene_stats_uri)
  has_expression <- nzchar(expression_uri)
  if (!has_gene_stats && !has_expression) {
    return(fail(mcp_tool_error(
      "One of gene_stats_uri or expression_uri + experiment_design_uri is required.")))
  }
  if (has_expression && !nzchar(design_uri)) {
    return(fail(mcp_tool_error(
      "experiment_design_uri is required when expression_uri is provided.")))
  }
  if (has_expression && "gsa" %in% pa_methods_vec) {
    return(fail(mcp_tool_error(paste(
      "Method 'gsa' is not supported for consensus analysis with",
      "expression input.",
      "Use fgsea, ora, ks, wilcox, spia, cepaORA, or cepaGSA."))))
  }

  input_type <- if (has_expression) "expression" else "gene_stats"
  has_pairs <- FALSE
  pairs <- NULL

  if (identical(input_type, "gene_stats")) {
    tmp_gene_stats <- tryCatch(fetch_to_tempfile(gene_stats_uri),
                                error = function(e) e)
    if (inherits(tmp_gene_stats, "error")) {
      return(fail(mcp_tool_error(paste("Failed to fetch gene_stats_uri:",
                                        conditionMessage(tmp_gene_stats)))))
    }
    vr <- validate_gene_stats(tmp_gene_stats)
    if (!vr$valid) {
      return(fail(mcp_tool_error(vr$issues,
                                  expected_format = .GENE_STATS_FORMAT)))
    }
  } else {
    tmp_expr <- tryCatch(fetch_to_tempfile(expression_uri),
                         error = function(e) e)
    if (inherits(tmp_expr, "error")) {
      return(fail(mcp_tool_error(paste("Failed to fetch expression_uri:",
                                        conditionMessage(tmp_expr)))))
    }
    vr_expr <- validate_expression_matrix(tmp_expr)
    if (!vr_expr$valid) {
      return(fail(mcp_tool_error(vr_expr$issues)))
    }
    tmp_design_csv <- tryCatch(fetch_to_tempfile(design_uri),
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
    pair_vals <- lapply(design, function(d) d$pair)
    has_pairs <- !any(vapply(pair_vals, is.null, logical(1L)))
    pairs <- if (has_pairs) unlist(pair_vals) else NULL
    if (!nzchar(contrast)) {
      groups <- unique(vapply(design, `[[`, character(1L), "group"))
      if (length(groups) == 2L) {
        contrast <- paste0(groups[[2L]], " - ", groups[[1L]])
      } else {
        return(fail(mcp_tool_error(sprintf(
          paste("contrast is required when more than 2 groups are",
                "present. Groups: %s"),
          paste(groups, collapse = ", ")))))
      }
    }
    if (!nzchar(de_method)) de_method <- "limma"
    if (!de_method %in% c("limma", "DESeq2", "edgeR")) {
      return(fail(mcp_tool_error(sprintf(
        "de_method must be 'limma', 'DESeq2', or 'edgeR'. Got: '%s'",
        de_method))))
    }
    tmp_design_json <- tempfile(fileext = ".json")
    jsonlite::write_json(design, tmp_design_json, auto_unbox = TRUE)
  }

  run <- make_run_dir()
  base_name <- paste0("consensus_", tolower(method))
  csv_fname <- paste0(base_name, ".csv")
  rds_path <- file.path(run$dir, paste0(base_name, ".rds"))
  csv_path <- file.path(run$dir, csv_fname)
  csv_url <- paste0(base_url(), "/results/", run$run_id, "/", csv_fname)

  job_params <- list(
    input_type = input_type,
    database = database,
    org = org,
    namespace = namespace,
    pa_methods = as.list(pa_methods_vec),
    method = method,
    use_fdr = isTRUE(use_fdr),
    weights = if (!is.null(weights) && length(weights) > 0L) {
                as.list(as.numeric(unlist(weights)))
              } else NULL,
    rank_by = rank_by,
    databases_dir = file.path(results_dir(), "gene_set_cache"),
    rds_path = rds_path,
    csv_path = csv_path
  )
  if (identical(input_type, "gene_stats")) {
    job_params$gene_stats_path <- tmp_gene_stats
  } else {
    job_params$expr_path <- tmp_expr
    job_params$design_json_path <- tmp_design_json
    job_params$de_method <- de_method
    job_params$contrast <- contrast
    job_params$has_pairs <- has_pairs
    job_params$pairs <- pairs
  }
  job <- make_job_script(run$dir, base_name, "consensus_analysis", job_params)

  list(error = NULL,
       script_path = job$script_path,
       job_name = base_name,
       csv_path = csv_path,
       csv_url = csv_url,
       csv_fname = csv_fname,
       method = method,
       pa_methods = pa_methods_vec,
       run_id = run$run_id,
       tmp_files = list(tmp_gene_stats, tmp_expr,
                        tmp_design_csv, tmp_design_json))
}

consensus_build_response <- function(job_result, prep) {
  if (!job_result$success) {
    return(mcp_tool_error(paste("Consensus analysis failed:",
                                 job_result$stderr)))
  }
  csv_df <- tryCatch(utils::read.csv(prep$csv_path,
                                      stringsAsFactors = FALSE),
                     error = function(e) NULL)
  n_pathways <- if (!is.null(csv_df)) nrow(csv_df) else NA_integer_
  n_sig <- if (!is.null(csv_df) && "pFDR" %in% colnames(csv_df)) {
    sum(!is.na(csv_df$pFDR) & csv_df$pFDR < 0.05, na.rm = TRUE)
  } else {
    NA_integer_
  }
  metadata_json <- jsonlite::toJSON(list(
    result_type = "consensus_result",
    method = prep$method,
    pa_methods = prep$pa_methods,
    n_pathways = n_pathways,
    n_significant = n_sig,
    run_id = prep$run_id
  ), auto_unbox = TRUE)

  list(
    content = list(
      mcpserver::response_text(metadata_json),
      mcpserver::response_resource_link(
        uri = prep$csv_url,
        name = prep$csv_fname,
        mime_type = "text/csv")
    ),
    isError = FALSE
  )
}
