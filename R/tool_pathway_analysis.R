.PATHWAY_METHODS <- c("spia", "cepaORA", "cepaGSA")
.GENE_STATS_FORMAT <- paste(
  "CSV or TSV with exactly these column names (case-sensitive):",
  "'id' (gene identifier), 'logFC' (log fold-change, numeric),",
  "'pvalue' (p-value, numeric, 0-1).",
  "One row per gene. No duplicate IDs.")

validate_org <- function(org) grepl("^[a-z]{3,5}$", org)

tool_run_pathway_analysis <- function() {
  mcpserver::new_tool(
    name = "run_pathway_analysis",
    description = paste(
      "Run gene set or pathway enrichment analysis.",
      "Accepted inputs:",
      "(a) gene_stats_uri - CSV with id/logFC/pvalue from a DE analysis;",
      "(b) gene_list - array of significant gene Entrez IDs;",
      "(c) expression_uri + experiment_design_uri - raw expression matrix (for GSA).",
      "Supports KEGG and GO databases.",
      "An elicitation-capable client will be prompted interactively for",
      "databases, methods, GO namespace, and organism when not provided.",
      "All *_uri parameters must be URLs from the platform - do not",
      "construct or modify them."),
    input_schema = mcpserver::schema(list(
      gene_stats_uri = mcpserver::property_string(
        description = "URL to a gene stats CSV (id, logFC, pvalue)."),
      gene_list = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Array of significant gene Entrez IDs for ORA-only analysis."),
      background_gene_list = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Optional background gene IDs for ORA."),
      expression_uri = mcpserver::property_string(
        description = "URL to expression matrix CSV. Required for GSA."),
      experiment_design_uri = mcpserver::property_string(
        description = paste("URL to experiment design CSV.",
                            .DESIGN_CSV_FORMAT)),
      contrast = mcpserver::property_string(
        description = "Contrast string, e.g. 'Treatment - Control'."),
      de_method = mcpserver::property_enum(
        values = c("limma", "DESeq2", "edgeR"),
        description = "DE method when using expression input."),
      databases = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = paste("Pathway databases: KEGG, GO.",
                            "Omit entirely to trigger interactive selection.")),
      methods = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = paste(
          "Analysis methods: ora, fgsea, gsa, ks, wilcox, spia, cepaORA, cepaGSA.",
          "Omit entirely to trigger interactive selection.")),
      org = mcpserver::property_string(
        description = paste("KEGG organism code, e.g. hsa, mmu, rno.",
                            "Omit entirely to trigger interactive selection.")),
      namespace = mcpserver::property_string(
        description = paste(
          "GO namespace: biological_process, molecular_function,",
          "cellular_component.",
          "Omit entirely to trigger interactive selection."))
    )),
    annotations = list(
      readOnlyHint = FALSE,
      destructiveHint = FALSE,
      idempotentHint = FALSE,
      openWorldHint = TRUE,
      title = "Run Pathway Analysis"
    ),
    bidirectional = TRUE,
    handler = run_pathway_analysis_handler
  )
}

run_pathway_analysis_handler <- function(args, ctx) {
  filled <- elicit_pa_args(args, ctx)
  if (is_tool_error(filled)) return(filled)
  prep <- pa_prepare(filled,
                     filled$databases, filled$methods,
                     filled$org, filled$namespace)
  if (!is.null(prep$error)) return(prep$error)
  jobs <- lapply(prep$combos, function(combo) {
    run_job_sync(combo$script_path, combo$job_name)
  })
  safe_unlink_all(prep$tmp_files)
  pa_build_response(jobs, prep)
}

# ── Elicitation flow ─────────────────────────────────────────────────────
elicit_pa_args <- function(args, ctx) {
  databases <- if (is.character(args$databases)) as.list(args$databases)
               else args$databases %||% list()
  methods   <- if (is.character(args$methods)) as.list(args$methods)
               else args$methods %||% list()
  org       <- trimws(args$org %||% "")
  namespace <- trimws(args$namespace %||% "")

  has_expression <- nzchar(trimws(args$expression_uri %||% ""))
  has_gene_list  <- length(args$gene_list %||% list()) > 0L
  input_type <- if (has_expression) "expression"
                else if (has_gene_list) "gene_list"
                else "gene_stats"

  caps <- ctx$client_capabilities %||% list()
  can_elicit <- !is.null(caps$elicitation)

  if (length(databases) == 0L || length(methods) == 0L) {
    if (!can_elicit) {
      return(mcp_tool_error(
        "databases and methods are required.",
        hint = paste("Provide them explicitly, or connect with a client",
                     "that supports MCP elicitation.")))
    }
    sch <- pa_db_methods_schema(input_type)
    res <- tryCatch(ctx$request_elicitation(
      message = pa_db_methods_message(input_type),
      requested_schema = sch), error = function(e) e)
    if (inherits(res, "error")) {
      return(mcp_tool_error(paste("Elicitation failed:",
                                   conditionMessage(res))))
    }
    if (!identical(res$action %||% "accept", "accept")) {
      return(mcp_tool_error("User declined to choose databases/methods."))
    }
    databases <- as.list(res$content$databases %||% list("KEGG"))
    methods   <- as.list(res$content$methods   %||% list("fgsea"))
  }

  if ("GO" %in% unlist(databases) && !nzchar(namespace)) {
    if (!can_elicit) {
      namespace <- "biological_process"
    } else {
      res <- tryCatch(ctx$request_elicitation(
        message = "Which GO namespace would you like to analyse?",
        requested_schema = pa_namespace_schema()),
        error = function(e) e)
      if (inherits(res, "error")) {
        return(mcp_tool_error(paste("Elicitation failed:",
                                     conditionMessage(res))))
      }
      namespace <- res$content$namespace %||% "biological_process"
    }
  }
  if (!nzchar(namespace)) namespace <- "biological_process"

  if (!nzchar(org)) {
    if (!can_elicit) {
      return(mcp_tool_error(
        "org is required (KEGG organism code).",
        hint = "Common values: hsa (Human), mmu (Mouse), rno (Rat)."))
    }
    res <- tryCatch(ctx$request_elicitation(
      message = paste(
        "Which organism are these genes from?",
        "Enter the KEGG 3-letter code",
        "(e.g. hsa = Human, mmu = Mouse, rno = Rat)."),
      requested_schema = pa_org_schema()),
      error = function(e) e)
    if (inherits(res, "error")) {
      return(mcp_tool_error(paste("Elicitation failed:",
                                   conditionMessage(res))))
    }
    org <- trimws(res$content$org %||% "hsa")
  }

  args$databases <- databases
  args$methods <- methods
  args$org <- org
  args$namespace <- namespace
  args
}

pa_db_methods_schema <- function(input_type) {
  db_field <- list(
    type = "array", title = "Pathway Databases", minItems = 1L,
    items = list(enum = I(c("KEGG", "GO"))),
    default = I("KEGG")
  )
  method_field <- if (identical(input_type, "gene_list")) {
    list(
      type = "array", title = "Analysis Methods", minItems = 1L,
      items = list(enum = I(c("ora", "cepaORA"))),
      default = I("ora")
    )
  } else if (identical(input_type, "expression")) {
    list(
      type = "array", title = "Analysis Methods", minItems = 1L,
      items = list(enum = I("gsa")),
      default = I("gsa")
    )
  } else {
    list(
      type = "array", title = "Analysis Methods", minItems = 1L,
      items = list(enum = I(c("ora", "fgsea", "ks", "wilcox",
                              "spia", "cepaORA", "cepaGSA"))),
      default = I("fgsea")
    )
  }
  list(
    type = "object",
    required = I(c("databases", "methods")),
    properties = list(databases = db_field, methods = method_field)
  )
}

pa_db_methods_message <- function(input_type) {
  if (identical(input_type, "gene_list")) {
    "Only ORA-compatible methods are available for a gene list."
  } else if (identical(input_type, "expression")) {
    paste("Select pathway database(s).",
          "Only GSA is available for raw expression input.")
  } else {
    paste("Select pathway database(s) and analysis methods.",
          "(GSA requires raw expression input and is not available for",
          "pre-computed gene statistics.)")
  }
}

pa_namespace_schema <- function() {
  list(
    type = "object",
    required = I("namespace"),
    properties = list(
      namespace = list(
        type = "string", title = "GO Namespace",
        enum = I(c("biological_process",
                   "molecular_function",
                   "cellular_component")),
        default = "biological_process"
      )
    )
  )
}

pa_org_schema <- function() {
  list(
    type = "object",
    required = I("org"),
    properties = list(
      org = list(
        type = "string",
        title = "Organism (KEGG code)",
        description = paste("3-letter KEGG organism code.",
                            "Common: hsa (Human), mmu (Mouse), rno (Rat)."),
        default = "hsa"
      )
    )
  )
}

# ── Prepare: validate + fetch + build job combos ─────────────────────────
pa_prepare <- function(args, databases, methods, org, namespace) {
  gene_stats_uri <- trimws(args$gene_stats_uri %||% "")
  gene_list <- args$gene_list %||% list()
  bg_gene_list <- args$background_gene_list %||% list()
  expression_uri <- trimws(args$expression_uri %||% "")
  design_uri <- trimws(args$experiment_design_uri %||% "")
  contrast <- trimws(args$contrast %||% "")
  de_method <- trimws(args$de_method %||% "limma")

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

  valid_methods <- c("ora", "fgsea", "gsa", "ks", "wilcox",
                     "spia", "cepaORA", "cepaGSA")
  invalid_methods <- setdiff(unlist(methods), valid_methods)
  if (length(invalid_methods) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid method(s): %s. Valid: %s",
      paste(invalid_methods, collapse = ", "),
      paste(valid_methods, collapse = ", ")))))
  }

  valid_dbs <- c("KEGG", "GO")
  invalid_dbs <- setdiff(unlist(databases), valid_dbs)
  if (length(invalid_dbs) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Invalid database(s): %s. Valid: KEGG, GO",
      paste(invalid_dbs, collapse = ", ")))))
  }

  if (!namespace %in% c("biological_process",
                         "molecular_function",
                         "cellular_component")) {
    return(fail(mcp_tool_error(sprintf(
      paste("Invalid GO namespace: '%s'.",
            "Must be: biological_process, molecular_function,",
            "cellular_component."),
      namespace))))
  }

  has_expression <- nzchar(expression_uri)
  has_gene_stats <- nzchar(gene_stats_uri)
  has_gene_list <- length(gene_list) > 0L

  if (!has_expression && !has_gene_stats && !has_gene_list) {
    return(fail(mcp_tool_error(
      "One of gene_stats_uri, gene_list, or expression_uri + experiment_design_uri is required.")))
  }
  if (has_expression && !nzchar(design_uri)) {
    return(fail(mcp_tool_error(
      "experiment_design_uri is required when expression_uri is provided.")))
  }

  input_type <- if (has_expression) "expression"
                else if (has_gene_stats) "gene_stats"
                else "gene_list"

  if (identical(input_type, "gene_stats") && "gsa" %in% unlist(methods)) {
    return(fail(mcp_tool_error(paste(
      "Method 'gsa' requires raw expression data",
      "(expression_uri + experiment_design_uri),",
      "not pre-computed gene statistics. Use fgsea, ora, ks, wilcox,",
      "spia, cepaORA, or cepaGSA with gene_stats_uri."))))
  }
  if (identical(input_type, "gene_list")) {
    incompatible <- setdiff(unlist(methods), c("ora", "cepaORA"))
    if (length(incompatible) > 0L) {
      return(fail(mcp_tool_error(sprintf(
        paste("Gene list input only supports: ora, cepaORA.",
              "Incompatible method(s): %s.",
              "Provide gene_stats_uri for other methods."),
        paste(incompatible, collapse = ", ")))))
    }
  }
  if (identical(input_type, "expression")) {
    incompatible <- setdiff(unlist(methods), "gsa")
    if (length(incompatible) > 0L) {
      return(fail(mcp_tool_error(sprintf(
        paste("Expression input only supports: gsa.",
              "Incompatible method(s): %s.",
              "Run DE analysis first and use gene_stats_uri."),
        paste(incompatible, collapse = ", ")))))
    }
    if (!nzchar(de_method)) de_method <- "limma"
    if (!de_method %in% c("limma", "DESeq2", "edgeR")) {
      return(fail(mcp_tool_error(sprintf(
        "de_method must be 'limma', 'DESeq2', or 'edgeR'. Got: '%s'",
        de_method))))
    }
  }

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
  } else if (identical(input_type, "expression")) {
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
          paste("contrast is required when more than 2 groups are present.",
                "Groups found: %s"),
          paste(groups, collapse = ", ")),
          expected_format = "A contrast string, e.g. 'Treated - Control'")))
      }
    }
    tmp_design_json <- tempfile(fileext = ".json")
    jsonlite::write_json(design, tmp_design_json, auto_unbox = TRUE)
  }

  run <- make_run_dir()
  skipped_kegg_only <- character(0L)
  combos <- list()

  for (db in unlist(databases)) {
    for (m in unlist(methods)) {
      if (m %in% .PATHWAY_METHODS && db != "KEGG") {
        if (!m %in% skipped_kegg_only) {
          skipped_kegg_only <- c(skipped_kegg_only, m)
        }
        next
      }
      base_name <- paste0("pa_", tolower(db), "_", m)
      rds_path <- file.path(run$dir, paste0(base_name, ".rds"))
      csv_fname <- paste0(base_name, ".csv")
      csv_path <- file.path(run$dir, csv_fname)
      csv_url <- paste0(base_url(), "/results/", run$run_id, "/", csv_fname)
      job_params <- list(
        input_type = input_type,
        method = m,
        database = db,
        org = org,
        namespace = namespace,
        databases_dir = file.path(results_dir(), "gene_set_cache"),
        rds_path = rds_path,
        csv_path = csv_path
      )
      if (identical(input_type, "gene_stats")) {
        job_params$gene_stats_path <- tmp_gene_stats
      } else if (identical(input_type, "gene_list")) {
        job_params$gene_ids <- as.list(unlist(gene_list))
        job_params$background_gene_ids <-
          if (length(bg_gene_list) > 0L) as.list(unlist(bg_gene_list))
          else NULL
      } else {
        job_params$expr_path <- tmp_expr
        job_params$design_json_path <- tmp_design_json
        job_params$de_method <- de_method
        job_params$contrast <- contrast
        job_params$has_pairs <- has_pairs
        job_params$pairs <- pairs
      }
      job <- make_job_script(run$dir, base_name, "pathway_analysis", job_params)
      combos[[length(combos) + 1L]] <- list(
        db = db, method = m, job_name = base_name,
        script_path = job$script_path,
        csv_url = csv_url,
        csv_fname = csv_fname
      )
    }
  }

  if (length(combos) == 0L) {
    return(fail(mcp_tool_error(
      paste("No valid jobs to run. Methods spia/cepaORA/cepaGSA are",
            "KEGG-only and cannot be used with GO."),
      hint = "Use KEGG database with these methods, or choose fgsea/ora/ks/wilcox.")))
  }

  list(
    error = NULL,
    combos = combos,
    skipped_kegg_only = skipped_kegg_only,
    databases = databases,
    org = org,
    run_id = run$run_id,
    tmp_files = list(tmp_gene_stats, tmp_expr,
                     tmp_design_csv, tmp_design_json)
  )
}

pa_build_response <- function(job_results, prep) {
  n_jobs <- length(job_results)
  n_ok <- sum(vapply(job_results, `[[`, logical(1L), "success"))
  if (n_ok == 0L) {
    msgs <- vapply(job_results, function(r) {
      sprintf("%s: %s", r$job_name, trimws(r$stderr))
    }, character(1L))
    return(mcp_tool_error(paste(msgs, collapse = "\n")))
  }

  failed_list <- lapply(
    Filter(function(r) !r$success, job_results),
    function(r) list(job = r$job_name, error = trimws(r$stderr))
  )

  response_data <- list(
    result_type = "pa_result",
    databases = unlist(prep$databases),
    org = prep$org,
    n_jobs_ok = n_ok,
    run_id = prep$run_id
  )
  if (length(prep$skipped_kegg_only) > 0L) {
    response_data$note <- sprintf(
      "Methods %s are KEGG-only and were skipped for GO database.",
      paste(prep$skipped_kegg_only, collapse = ", "))
  }
  if (length(failed_list) > 0L) response_data$failed <- failed_list

  metadata_json <- jsonlite::toJSON(response_data, auto_unbox = TRUE)
  content_items <- list(mcpserver::response_text(metadata_json))
  for (i in seq_along(job_results)) {
    if (job_results[[i]]$success) {
      content_items[[length(content_items) + 1L]] <-
        mcpserver::response_resource_link(
          uri = prep$combos[[i]]$csv_url,
          name = prep$combos[[i]]$csv_fname,
          mime_type = "text/csv")
    }
  }
  list(content = content_items, isError = FALSE)
}
