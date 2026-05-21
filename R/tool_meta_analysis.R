.PATHWAY_METHODS_META <- c("spia", "cepaORA", "cepaGSA")

tool_run_meta_analysis <- function() {
  mcpserver::new_tool(
    name = "run_meta_analysis",
    description = paste(
      "Run meta-analysis across 2 or more independent studies. Two modes:",
      "(a) DE meta-analysis: provide 'studies' array, each with",
      "expression_uri + experiment_design_uri - runs DE on each study",
      "then combines gene-level statistics;",
      "(b) PA meta-analysis: provide 'gene_stats_uris' array of per-study",
      "gene stats CSVs - runs the same PA method on each study then",
      "combines pathway scores.",
      "An elicitation-capable client will be prompted interactively for",
      "DE method, PA database/method, organism, GO namespace, and",
      "meta-analysis combining methods when not provided."),
    input_schema = mcpserver::schema(list(
      studies = mcpserver::property_array(
        items = mcpserver::property_object(list(
          expression_uri = mcpserver::property_string(),
          experiment_design_uri = mcpserver::property_string(),
          contrast = mcpserver::property_string()
        ), additional_properties = TRUE),
        description = "Array of studies for DE meta-analysis."),
      gene_stats_uris = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Array of URLs to per-study gene stats CSVs."),
      de_method = mcpserver::property_enum(
        values = c("limma", "DESeq2", "edgeR"),
        description = "DE method for each study in DE meta-analysis."),
      database = mcpserver::property_enum(
        values = c("KEGG", "GO"),
        description = "Pathway database for PA meta-analysis."),
      pa_method = mcpserver::property_string(
        description = "Single PA method to run on each study for PA meta."),
      methods = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Combining methods: fisher, stouffer, addCLT, geoMean, minP, REML."),
      org = mcpserver::property_string(
        description = "KEGG organism code, e.g. hsa, mmu, rno."),
      namespace = mcpserver::property_enum(
        values = c("biological_process",
                   "molecular_function",
                   "cellular_component"),
        description = "GO namespace.")
    )),
    annotations = list(
      readOnlyHint = FALSE,
      destructiveHint = FALSE,
      idempotentHint = FALSE,
      openWorldHint = TRUE,
      title = "Run Meta-Analysis"
    ),
    bidirectional = TRUE,
    handler = run_meta_handler
  )
}

run_meta_handler <- function(args, ctx) {
  studies <- args$studies %||% list()
  gene_stats_uris <- args$gene_stats_uris %||% list()
  mode <- if (length(studies) >= 2L) "de"
          else if (length(gene_stats_uris) >= 2L) "pa"
          else NA_character_
  if (is.na(mode)) {
    return(mcp_tool_error(paste(
      "Provide either 'studies' (>= 2 items) for DE meta-analysis or",
      "'gene_stats_uris' (>= 2 items) for PA meta-analysis.")))
  }
  filled <- elicit_meta_args(args, ctx, mode)
  if (is_tool_error(filled)) return(filled)
  prep <- if (identical(mode, "de")) {
    meta_de_prepare(filled, filled$de_method, filled$methods)
  } else {
    meta_pa_prepare(filled,
                    filled$database, filled$pa_method,
                    filled$org, filled$namespace, filled$methods)
  }
  if (!is.null(prep$error)) return(prep$error)
  all_results <- lapply(prep$jobs, function(j) {
    r <- run_job_sync(j$script_path, j$job_name)
    list(success = r$success, job_name = j$job_name, stderr = r$stderr)
  })
  safe_unlink_all(prep$tmp_files)
  meta_build_response(all_results, prep$job_meta, mode)
}

elicit_meta_args <- function(args, ctx, mode) {
  de_method <- trimws(args$de_method %||% "")
  pa_method <- trimws(args$pa_method %||% "")
  database <- trimws(args$database %||% "")
  methods <- if (is.character(args$methods)) as.list(args$methods)
             else args$methods %||% list()
  org <- trimws(args$org %||% "")
  namespace <- trimws(args$namespace %||% "")

  caps <- ctx$client_capabilities %||% list()
  can_elicit <- !is.null(caps$elicitation)

  if (identical(mode, "de")) {
    if (!nzchar(de_method) && can_elicit) {
      schema <- list(
        type = "object", required = I("method"),
        properties = list(
          method = list(type = "string", title = "DE Method",
                        enum = I(c("limma", "DESeq2", "edgeR")),
                        default = "limma")
        )
      )
      res <- tryCatch(ctx$request_elicitation(
        message = "Which DE method to use for each study?",
        requested_schema = schema), error = function(e) e)
      if (!inherits(res, "error") &&
          identical(res$action %||% "accept", "accept")) {
        de_method <- res$content$method %||% "limma"
      }
    }
    if (!nzchar(de_method)) de_method <- "limma"
  } else {
    if ((!nzchar(database) || !nzchar(pa_method)) && can_elicit) {
      schema <- list(
        type = "object",
        required = I(c("database", "pa_method")),
        properties = list(
          database = list(type = "string", title = "Pathway Database",
                          enum = I(c("KEGG", "GO")), default = "KEGG"),
          pa_method = list(
            type = "string", title = "Analysis Method",
            enum = I(c("ora", "fgsea", "ks", "wilcox",
                       "spia", "cepaORA", "cepaGSA")),
            default = "fgsea")
        )
      )
      res <- tryCatch(ctx$request_elicitation(
        message = paste("Select pathway database and one analysis method",
                        "to run on each study."),
        requested_schema = schema), error = function(e) e)
      if (!inherits(res, "error") &&
          identical(res$action %||% "accept", "accept")) {
        database <- res$content$database %||% "KEGG"
        pa_method <- res$content$pa_method %||% "fgsea"
      }
    }
    if (identical(database, "GO") && !nzchar(namespace) && can_elicit) {
      schema <- list(
        type = "object", required = I("namespace"),
        properties = list(
          namespace = list(type = "string", title = "GO Namespace",
                           enum = I(c("biological_process",
                                      "molecular_function",
                                      "cellular_component")),
                           default = "biological_process")
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
          org = list(type = "string", title = "Organism (KEGG code)",
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
  }

  if (length(methods) == 0L && can_elicit) {
    schema <- list(
      type = "object", required = I("methods"),
      properties = list(
        methods = list(
          type = "array",
          title = "Meta-analysis Combining Methods",
          minItems = 1L,
          items = list(enum = I(c("fisher", "stouffer", "addCLT",
                                  "geoMean", "minP", "REML"))),
          default = I("fisher")
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = "Select one or more meta-analysis combining methods.",
      requested_schema = schema), error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      methods <- as.list(res$content$methods %||% list("fisher"))
    }
  }
  if (length(methods) == 0L) {
    return(mcp_tool_error("'methods' is required (meta combining methods)."))
  }

  args$de_method <- de_method
  args$pa_method <- pa_method
  args$database <- database
  args$methods <- methods
  args$org <- org
  args$namespace <- namespace
  args
}

meta_de_prepare <- function(args, de_method, meta_methods) {
  studies <- args$studies %||% list()
  tmp_files <- list()
  fail <- function(resp) {
    safe_unlink_all(tmp_files)
    list(error = resp)
  }

  if (length(studies) < 2L) {
    return(fail(mcp_tool_error(
      "At least 2 studies are required for DE meta-analysis.")))
  }
  if (!de_method %in% c("limma", "DESeq2", "edgeR")) {
    return(fail(mcp_tool_error(sprintf(
      "de_method must be 'limma', 'DESeq2', or 'edgeR'. Got: '%s'",
      de_method))))
  }
  valid_meta <- c("fisher", "stouffer", "addCLT", "geoMean", "minP", "REML")
  invalid <- setdiff(unlist(meta_methods), valid_meta)
  if (length(invalid) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid meta-analysis method(s): %s. Valid: %s",
      paste(invalid, collapse = ", "),
      paste(valid_meta, collapse = ", ")))))
  }

  study_configs <- list()
  for (i in seq_along(studies)) {
    study <- studies[[i]]
    expr_uri <- trimws(study$expression_uri %||% "")
    design_uri <- trimws(study$experiment_design_uri %||% "")
    contrast <- trimws(study$contrast %||% "")
    if (!nzchar(expr_uri)) {
      return(fail(mcp_tool_error(sprintf("Study %d: expression_uri is required.", i))))
    }
    if (!nzchar(design_uri)) {
      return(fail(mcp_tool_error(sprintf("Study %d: experiment_design_uri is required.", i))))
    }

    tmp_expr <- tryCatch(fetch_to_tempfile(expr_uri),
                         error = function(e) e)
    if (inherits(tmp_expr, "error")) {
      return(fail(mcp_tool_error(sprintf("Study %d: failed to fetch expression_uri: %s",
                                           i, conditionMessage(tmp_expr)))))
    }
    tmp_files[[length(tmp_files) + 1L]] <- tmp_expr
    vr_expr <- validate_expression_matrix(tmp_expr)
    if (!vr_expr$valid) {
      return(fail(mcp_tool_error(sprintf("Study %d: %s", i,
                                          paste(vr_expr$issues, collapse = "; ")))))
    }
    tmp_design_csv <- tryCatch(fetch_to_tempfile(design_uri),
                               error = function(e) e)
    if (inherits(tmp_design_csv, "error")) {
      return(fail(mcp_tool_error(sprintf("Study %d: failed to fetch experiment_design_uri", i))))
    }
    tmp_files[[length(tmp_files) + 1L]] <- tmp_design_csv
    design <- tryCatch(parse_design_csv(tmp_design_csv),
                       error = function(e) e)
    if (inherits(design, "error")) {
      return(fail(mcp_tool_error(sprintf("Study %d: %s", i,
                                          conditionMessage(design)))))
    }
    groups <- unique(vapply(design, `[[`, character(1L), "group"))
    if (!nzchar(contrast)) {
      if (length(groups) == 2L) {
        contrast <- paste0(groups[[2L]], " - ", groups[[1L]])
      } else {
        return(fail(mcp_tool_error(sprintf(
          paste("Study %d: contrast is required when more than 2 groups.",
                "Groups: %s"),
          i, paste(groups, collapse = ", ")))))
      }
    }
    pair_vals <- lapply(design, function(d) d$pair)
    has_pairs <- !any(vapply(pair_vals, is.null, logical(1L)))
    pairs <- if (has_pairs) unlist(pair_vals) else NULL
    tmp_design_json <- tempfile(fileext = ".json")
    jsonlite::write_json(design, tmp_design_json, auto_unbox = TRUE)
    tmp_files[[length(tmp_files) + 1L]] <- tmp_design_json
    study_configs[[i]] <- list(
      expr_path = tmp_expr,
      design_path = tmp_design_json,
      contrast = contrast,
      has_pairs = has_pairs,
      pairs = pairs
    )
  }

  run <- make_run_dir()
  jobs <- list()
  job_meta <- list()
  for (mm in unlist(meta_methods)) {
    base_name <- paste0("meta_de_", de_method, "_", mm)
    csv_fname <- paste0(base_name, ".csv")
    rds_path <- file.path(run$dir, paste0(base_name, ".rds"))
    csv_path <- file.path(run$dir, csv_fname)
    csv_url <- paste0(base_url(), "/results/", run$run_id, "/", csv_fname)
    job <- make_job_script(run$dir, base_name, "meta_de_analysis", list(
      studies = study_configs,
      de_method = de_method,
      meta_method = mm,
      rds_path = rds_path,
      csv_path = csv_path
    ))
    jobs[[length(jobs) + 1L]] <- list(job_name = base_name,
                                       script_path = job$script_path)
    job_meta[[length(job_meta) + 1L]] <- list(
      csv_url = csv_url, csv_fname = csv_fname,
      description = sprintf("DE meta-analysis results (%s)", mm)
    )
  }
  list(error = NULL, jobs = jobs, job_meta = job_meta,
       tmp_files = tmp_files, run_id = run$run_id)
}

meta_pa_prepare <- function(args, database, pa_method,
                            org, namespace, meta_methods) {
  gene_stats_uris <- args$gene_stats_uris %||% list()
  tmp_files <- list()
  fail <- function(resp) {
    safe_unlink_all(tmp_files)
    list(error = resp)
  }
  if (length(gene_stats_uris) < 2L) {
    return(fail(mcp_tool_error(
      "At least 2 gene_stats_uris are required for PA meta-analysis.")))
  }
  if (!validate_org(org)) {
    return(fail(mcp_tool_error(sprintf(
      paste("Invalid KEGG organism code: '%s'.",
            "Must be 3-5 lowercase letters."), org))))
  }
  if (!database %in% c("KEGG", "GO")) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid database: '%s'. Must be 'KEGG' or 'GO'.", database))))
  }
  valid_methods <- c("ora", "fgsea", "gsa", "ks", "wilcox",
                     "spia", "cepaORA", "cepaGSA")
  if (!pa_method %in% valid_methods) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid pa_method: '%s'. Valid: %s",
      pa_method, paste(valid_methods, collapse = ", ")))))
  }
  if (pa_method %in% .PATHWAY_METHODS_META && identical(database, "GO")) {
    return(fail(mcp_tool_error(sprintf(
      "Method '%s' is KEGG-only and cannot be used with GO database.",
      pa_method))))
  }
  valid_meta <- c("fisher", "stouffer", "addCLT", "geoMean", "minP", "REML")
  invalid <- setdiff(unlist(meta_methods), valid_meta)
  if (length(invalid) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid meta-analysis method(s): %s",
      paste(invalid, collapse = ", ")))))
  }
  if (!namespace %in% c("biological_process",
                         "molecular_function",
                         "cellular_component")) {
    namespace <- "biological_process"
  }

  gs_paths <- list()
  for (i in seq_along(gene_stats_uris)) {
    uri <- trimws(gene_stats_uris[[i]])
    tmp_gs <- tryCatch(fetch_to_tempfile(uri),
                       error = function(e) e)
    if (inherits(tmp_gs, "error")) {
      return(fail(mcp_tool_error(sprintf(
        "Study %d: failed to fetch gene_stats_uri: %s", i, uri))))
    }
    tmp_files[[length(tmp_files) + 1L]] <- tmp_gs
    vr <- validate_gene_stats(tmp_gs)
    if (!vr$valid) {
      return(fail(mcp_tool_error(sprintf("Study %d: %s", i,
                                          paste(vr$issues, collapse = "; ")),
                                  expected_format = .GENE_STATS_FORMAT)))
    }
    gs_paths[[length(gs_paths) + 1L]] <- tmp_gs
  }

  run <- make_run_dir()
  jobs <- list()
  job_meta <- list()
  for (mm in unlist(meta_methods)) {
    base_name <- paste0("meta_pa_", tolower(database), "_", mm)
    csv_fname <- paste0(base_name, ".csv")
    rds_path <- file.path(run$dir, paste0(base_name, ".rds"))
    csv_path <- file.path(run$dir, csv_fname)
    csv_url <- paste0(base_url(), "/results/", run$run_id, "/", csv_fname)
    job <- make_job_script(run$dir, base_name, "meta_pa_analysis", list(
      gene_stats_paths = gs_paths,
      pa_method = pa_method,
      database = database,
      org = org,
      namespace = namespace,
      meta_method = mm,
      databases_dir = file.path(results_dir(), "gene_set_cache"),
      rds_path = rds_path,
      csv_path = csv_path
    ))
    jobs[[length(jobs) + 1L]] <- list(job_name = base_name,
                                       script_path = job$script_path)
    job_meta[[length(job_meta) + 1L]] <- list(
      csv_url = csv_url, csv_fname = csv_fname,
      description = sprintf("PA meta-analysis results (%s)", mm)
    )
  }
  list(error = NULL, jobs = jobs, job_meta = job_meta,
       tmp_files = tmp_files, run_id = run$run_id)
}

meta_build_response <- function(all_results, job_meta, mode) {
  n_jobs <- length(all_results)
  n_ok <- sum(vapply(all_results, `[[`, logical(1L), "success"))
  result_type <- if (identical(mode, "de")) "meta_de_result"
                 else "meta_pa_result"
  if (n_ok == 0L) {
    msgs <- vapply(all_results, function(r) {
      sprintf("%s: %s", r$job_name, trimws(r$stderr))
    }, character(1L))
    return(mcp_tool_error(paste(msgs, collapse = "\n")))
  }
  failed_list <- lapply(
    Filter(function(r) !r$success, all_results),
    function(r) list(job = r$job_name, error = trimws(r$stderr))
  )
  response_data <- list(result_type = result_type, n_jobs_ok = n_ok)
  if (length(failed_list) > 0L) response_data$failed <- failed_list
  metadata_json <- jsonlite::toJSON(response_data, auto_unbox = TRUE)
  content_items <- list(mcpserver::response_text(metadata_json))
  for (i in seq_along(all_results)) {
    if (all_results[[i]]$success) {
      content_items[[length(content_items) + 1L]] <-
        mcpserver::response_resource_link(
          uri = job_meta[[i]]$csv_url,
          name = job_meta[[i]]$csv_fname,
          mime_type = "text/csv")
    }
  }
  list(content = content_items, isError = FALSE)
}
