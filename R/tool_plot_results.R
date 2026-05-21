.PLOT_DISPATCH <- list(
  volcano = c("de_result", "pa_result"),
  bar     = c("pa_result", "meta_pa_result",
              "consensus_result", "meta_de_result"),
  heatmap = c("de_result", "pa_result",
              "meta_pa_result", "meta_de_result"),
  forest  = c("pa_result", "meta_pa_result"),
  venn    = c("de_result", "pa_result", "meta_de_result"),
  network = c("pa_result"),
  kegg    = c("de_result"),
  ma      = c("de_result")
)

.PLOT_TYPE_TITLES <- list(
  volcano = "Volcano Plot",
  bar     = "Bar Chart",
  heatmap = "Heatmap",
  forest  = "Forest Plot",
  venn    = "Venn Diagram",
  network = "Pathway Network",
  kegg    = "KEGG Map",
  ma      = "MA Plot"
)

tool_plot_results <- function() {
  mcpserver::new_tool(
    name = "plot_results",
    description = paste(
      "Generate PNG visualizations from DE or pathway analysis result CSVs.",
      "Supported plot types: volcano, bar, heatmap, forest, venn, network,",
      "kegg, ma.",
      "Pass result_type from the analysis tool response.",
      "Elicitation-capable clients will be prompted for plot type,",
      "top_n_genes (DE heatmap), and top_n_pathways when not provided."),
    input_schema = mcpserver::schema(list(
      result_uri = mcpserver::property_string(
        description = "URL to a result CSV from a prior analysis tool."),
      result_uris = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Multiple result CSV URLs (heatmap, forest, venn)."),
      result_type = mcpserver::property_enum(
        values = c("de_result", "pa_result", "meta_de_result",
                   "meta_pa_result", "consensus_result"),
        description = "Type from analysis response.",
        required = TRUE),
      type = mcpserver::property_enum(
        values = names(.PLOT_DISPATCH),
        description = "Single plot type."),
      types = mcpserver::property_array(
        items = mcpserver::property_string(),
        description = "Multiple plot types to generate."),
      p_threshold = mcpserver::property_number(
        description = "Significance threshold."),
      use_fdr = mcpserver::property_boolean(
        description = "Use FDR-adjusted p-values for coloring/filtering."),
      log_fc_threshold = mcpserver::property_number(
        description = "Log fold-change threshold for volcano/MA plots."),
      kegg_pathway_id = mcpserver::property_string(
        description = "Required for kegg plot type (e.g. 'hsa04110')."),
      org = mcpserver::property_string(
        description = "KEGG organism code for network plot."),
      top_n_genes = mcpserver::property_integer(
        description = "Number of top DE genes for heatmap, ranked by pFDR.",
        minimum = 1L, maximum = 500L),
      top_n_pathways = mcpserver::property_integer(
        description = "Number of top pathways, ranked by pFDR.",
        minimum = 1L),
      file_name = mcpserver::property_string(
        description = "Optional filename suffix to distinguish outputs.")
    )),
    annotations = list(
      readOnlyHint = FALSE,
      destructiveHint = FALSE,
      idempotentHint = FALSE,
      openWorldHint = TRUE,
      title = "Plot Results"
    ),
    bidirectional = TRUE,
    handler = run_plot_results_handler
  )
}

run_plot_results_handler <- function(args, ctx) {
  filled <- elicit_plot_args(args, ctx)
  if (is_tool_error(filled)) return(filled)
  prep <- plot_results_prepare(filled)
  if (!is.null(prep$error)) return(prep$error)
  jobs <- lapply(prep$plot_items, function(item) {
    list(item = item, job = run_job_sync(item$script_path, item$job_name))
  })
  safe_unlink_all(prep$tmp_paths)
  plot_build_response(jobs)
}

elicit_plot_args <- function(args, ctx) {
  result_type <- trimws(args$result_type %||% "")
  if (!nzchar(result_type)) {
    return(mcp_tool_error("result_type is required"))
  }
  plot_type <- args$type
  plot_types <- args$types %||% list()
  if (!is.null(plot_type)) {
    plot_types <- c(list(plot_type), as.list(plot_types))
  }
  if (is.character(plot_types)) plot_types <- as.list(plot_types)

  caps <- ctx$client_capabilities %||% list()
  can_elicit <- !is.null(caps$elicitation)

  if (length(plot_types) == 0L) {
    valid_types <- names(Filter(
      function(allowed) result_type %in% allowed, .PLOT_DISPATCH))
    if (!can_elicit) {
      return(mcp_tool_error(sprintf(
        "type or types is required. Valid for '%s': %s",
        result_type, paste(valid_types, collapse = ", "))))
    }
    schema <- list(
      type = "object",
      required = I("types"),
      properties = list(
        types = list(
          type = "array", title = "Chart Types", minItems = 1L,
          items = list(enum = I(valid_types))
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = "Which visualizations would you like to generate?",
      requested_schema = schema),
      error = function(e) e)
    if (inherits(res, "error")) {
      return(mcp_tool_error(paste("Elicitation failed:",
                                   conditionMessage(res))))
    }
    if (!identical(res$action %||% "accept", "accept")) {
      return(mcp_tool_error("User declined to choose plot types."))
    }
    plot_types <- as.list(res$content$types %||% list())
  }
  args$types <- plot_types
  args$type <- NULL

  is_de <- result_type %in% c("de_result", "meta_de_result")
  pa_plot_types <- c("bar", "heatmap", "forest", "network")

  if ("heatmap" %in% unlist(plot_types) && is_de &&
      is.null(args$top_n_genes) && can_elicit) {
    schema <- list(
      type = "object",
      required = I("top_n_genes"),
      properties = list(
        top_n_genes = list(
          type = "integer",
          title = "Number of top genes to display",
          description = "Genes ranked by FDR-adjusted p-value.",
          default = 50L, minimum = 1L, maximum = 500L
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = paste(
        "How many top DE genes (ranked by pFDR)",
        "would you like to display in the heatmap?"),
      requested_schema = schema),
      error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      args$top_n_genes <- res$content$top_n_genes %||% 50L
    }
  }

  if (!is_de && any(unlist(plot_types) %in% pa_plot_types) &&
      is.null(args$top_n_pathways) && can_elicit) {
    schema <- list(
      type = "object",
      required = list(),
      properties = list(
        top_n_pathways = list(
          type = "integer",
          title = "Number of top pathways to display",
          description = "Pathways ranked by pFDR. Leave empty to show all.",
          minimum = 1L
        )
      )
    )
    res <- tryCatch(ctx$request_elicitation(
      message = paste(
        "How many top pathways (ranked by pFDR) would you like to display?",
        "Leave empty to show all."),
      requested_schema = schema),
      error = function(e) e)
    if (!inherits(res, "error") &&
        identical(res$action %||% "accept", "accept")) {
      args$top_n_pathways <- res$content$top_n_pathways
    }
  }
  args
}

plot_results_prepare <- function(args) {
  result_uri <- trimws(args$result_uri %||% "")
  result_uris <- args$result_uris %||% list()
  result_type <- trimws(args$result_type %||% "")
  plot_types <- args$types %||% list()
  if (is.character(plot_types)) plot_types <- as.list(plot_types)
  p_threshold <- args$p_threshold %||% 0.05
  use_fdr <- args$use_fdr %||% TRUE
  log_fc_threshold <- args$log_fc_threshold %||% 1.0
  kegg_pathway_id <- trimws(args$kegg_pathway_id %||% "")
  org <- args$org %||% "hsa"
  top_n_genes <- args$top_n_genes %||% 50L
  top_n_pathways <- args$top_n_pathways
  file_name <- trimws(args$file_name %||% "")

  tmp_paths <- list()
  fail <- function(resp) {
    safe_unlink_all(tmp_paths)
    list(error = resp)
  }

  if (!nzchar(result_type)) {
    return(fail(mcp_tool_error("result_type is required")))
  }
  if (length(plot_types) == 0L) {
    return(fail(mcp_tool_error("type or types is required")))
  }

  invalid_types <- setdiff(unlist(plot_types), names(.PLOT_DISPATCH))
  if (length(invalid_types) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Unknown plot type(s): %s",
      paste(invalid_types, collapse = ", ")))))
  }

  incompatible <- Filter(
    function(pt) !result_type %in% .PLOT_DISPATCH[[pt]],
    unlist(plot_types))
  if (length(incompatible) > 0L) {
    return(fail(mcp_tool_error(sprintf(
      "Plot type(s) not compatible with result_type '%s': %s",
      result_type, paste(incompatible, collapse = ", ")))))
  }

  if ("kegg" %in% unlist(plot_types) && !nzchar(kegg_pathway_id)) {
    return(fail(mcp_tool_error(
      "kegg_pathway_id is required for kegg plot type",
      hint = "Provide a KEGG pathway ID such as 'hsa04110'.")))
  }

  all_uris <- if (nzchar(result_uri)) {
    c(result_uri, unlist(result_uris))
  } else {
    unlist(result_uris)
  }
  if (length(all_uris) == 0L) {
    return(fail(mcp_tool_error("result_uri or result_uris is required")))
  }

  tmp_paths <- lapply(all_uris, function(uri) {
    tryCatch(fetch_to_tempfile(uri, suffix = ".csv"),
             error = function(e) e)
  })
  if (any(vapply(tmp_paths, inherits, logical(1L), "error"))) {
    errs <- vapply(Filter(function(x) inherits(x, "error"), tmp_paths),
                   conditionMessage, character(1L))
    return(fail(mcp_tool_error(paste("Failed to fetch one or more result CSVs:",
                                     paste(errs, collapse = "; ")))))
  }

  is_de <- result_type %in% c("de_result", "meta_de_result")
  if (is.null(top_n_genes) || isTRUE(top_n_genes == 0L)) {
    top_n_genes <- 50L
  }
  run <- make_run_dir()
  plot_items <- lapply(unlist(plot_types), function(pt) {
    png_fname <- if (nzchar(file_name)) {
      paste0(pt, "_", file_name, ".png")
    } else {
      paste0(pt, ".png")
    }
    png_path <- file.path(run$dir, png_fname)
    png_url <- paste0(base_url(), "/results/", run$run_id, "/", png_fname)
    job_name <- if (nzchar(file_name)) {
      paste0("plot_", pt, "_", file_name)
    } else {
      paste0("plot_", pt)
    }
    job <- make_job_script(run$dir, job_name, "plot_results", list(
      plot_type = pt,
      result_type = result_type,
      csv_paths = as.list(vapply(tmp_paths,
                                  function(p) p,
                                  character(1L))),
      is_de = is_de,
      p_threshold = p_threshold,
      use_fdr = isTRUE(use_fdr),
      log_fc_threshold = log_fc_threshold,
      kegg_pathway_id = kegg_pathway_id,
      org = org,
      top_n_genes = as.integer(top_n_genes),
      top_n_pathways = top_n_pathways,
      png_path = png_path
    ))
    list(plot_type = pt,
         job_name = job_name,
         png_url = png_url,
         png_fname = png_fname,
         script_path = job$script_path)
  })

  list(error = NULL,
       plot_items = plot_items,
       run_id = run$run_id,
       tmp_paths = tmp_paths)
}

plot_build_response <- function(jobs) {
  content_items <- list()
  for (entry in jobs) {
    item <- entry$item
    job <- entry$job
    if (!job$success) {
      content_items[[length(content_items) + 1L]] <-
        mcpserver::response_text(sprintf(
          "Failed to generate '%s' plot: %s",
          item$plot_type, trimws(job$stderr)))
    } else {
      content_items[[length(content_items) + 1L]] <-
        mcpserver::response_resource_link(
          uri = item$png_url,
          name = item$png_fname,
          mime_type = "image/png")
    }
  }
  if (length(content_items) == 0L) {
    return(mcp_tool_error("All plot generation jobs failed"))
  }
  list(content = content_items, isError = FALSE)
}
