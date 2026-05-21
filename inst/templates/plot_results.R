# Plot results template — sourced by a job script with `params` already set.
# One job = one plot type.
#
# params:
#   plot_type:        volcano | bar | heatmap | forest | venn | network | kegg | ma
#   result_type:      de_result | pa_result | meta_de_result | meta_pa_result | consensus_result
#   csv_paths:        character vector of local CSV paths (already fetched)
#   is_de:            logical
#   p_threshold:      numeric
#   use_fdr:          logical
#   log_fc_threshold: numeric
#   kegg_pathway_id:  string (required for kegg plot type)
#   org:              KEGG organism code (for network plot)
#   top_n_genes:      integer (for DE heatmap; default 50)
#   top_n_pathways:   integer or NULL (NULL = show all)
#   png_path:         output PNG file path

`%||%` <- function(a, b) if (!is.null(a)) a else b

library(ggplot2)
library(RCPA)

ts <- function() format(Sys.time(), "[%H:%M:%S]")

csv_paths   <- unlist(params$csv_paths)
is_de       <- isTRUE(params$is_de)
use_fdr_val <- isTRUE(params$use_fdr)
limit_val   <- if (!is.null(params$top_n_pathways)) as.numeric(params$top_n_pathways) else Inf
top_n_genes <- as.integer(params$top_n_genes %||% 50L)

cat(sprintf("%s plot_results.R  plot_type=%s  result_type=%s  csv_count=%d\n",
            ts(), params$plot_type, params$result_type, length(csv_paths)))

p <- switch(params$plot_type,

  volcano = if (is_de) {
    cat(sprintf("%s Reading DE results from %s\n", ts(), csv_paths[[1]]))
    rd <- read.csv(csv_paths[[1]], check.names = FALSE)
    RCPA::plotVolcanoDE(rd,
      pThreshold     = params$p_threshold,
      useFDR         = use_fdr_val,
      logFCThreshold = params$log_fc_threshold)
  } else {
    cat(sprintf("%s Reading pathway results from %s\n", ts(), csv_paths[[1]]))
    obj <- read.csv(csv_paths[[1]], check.names = FALSE)
    RCPA::plotVolcanoPathway(obj, pThreshold = params$p_threshold)
  },

  bar = {
    cat(sprintf("%s Reading results from %s\n", ts(), csv_paths[[1]]))
    obj <- read.csv(csv_paths[[1]], check.names = FALSE)
    if (!"p.value" %in% names(obj)) obj$p.value <- obj$pFDR
    RCPA::plotBarChart(list("Results" = obj),
      limit      = limit_val,
      pThreshold = params$p_threshold,
      useFDR     = use_fdr_val)
  },

  heatmap = if (is_de) {
    cat(sprintf("%s Reading %d DE result file(s) for heatmap\n", ts(), length(csv_paths)))
    rd_list   <- lapply(csv_paths, function(f) read.csv(f, check.names = FALSE))
    top_genes <- head(rd_list[[1]][order(rd_list[[1]]$pFDR), "ID"], top_n_genes)
    cat(sprintf("%s Displaying top %d genes\n", ts(), length(top_genes)))
    RCPA::plotDEGeneHeatmap(rd_list, genes = top_genes, useFDR = use_fdr_val)
  } else {
    cat(sprintf("%s Reading %d pathway result file(s) for heatmap\n", ts(), length(csv_paths)))
    results_list <- lapply(csv_paths, function(f) {
      df <- read.csv(f, check.names = FALSE)
      if (!"p.value" %in% names(df)) df$p.value <- df$pFDR
      df
    })
    top_ids <- if (!is.null(params$top_n_pathways)) {
      head(results_list[[1]][order(results_list[[1]]$pFDR), "ID"],
           as.integer(params$top_n_pathways))
    } else NULL
    RCPA::plotPathwayHeatmap(results_list, selectedPathways = top_ids, useFDR = use_fdr_val)
  },

  forest = {
    cat(sprintf("%s Reading %d result file(s) for forest plot\n", ts(), length(csv_paths)))
    results_list <- lapply(csv_paths, function(f) {
      df <- read.csv(f, check.names = FALSE)
      if (!"p.value" %in% names(df)) df$p.value <- df$pFDR
      df
    })
    top_ids <- if (!is.null(params$top_n_pathways)) {
      head(results_list[[1]][order(results_list[[1]]$pFDR), "ID"],
           as.integer(params$top_n_pathways))
    } else NULL
    RCPA::plotForest(results_list, selectedPathways = top_ids, useFDR = use_fdr_val)
  },

  venn = if (is_de) {
    cat(sprintf("%s Reading %d DE result file(s) for Venn diagram\n", ts(), length(csv_paths)))
    rd_list <- lapply(csv_paths, function(f) read.csv(f, check.names = FALSE))
    RCPA::plotVennDE(rd_list, pThreshold = params$p_threshold, useFDR = use_fdr_val)
  } else {
    cat(sprintf("%s Reading %d pathway result file(s) for Venn diagram\n", ts(), length(csv_paths)))
    results_list <- lapply(csv_paths, function(f) {
      df <- read.csv(f, check.names = FALSE)
      if (!"p.value" %in% names(df)) df$p.value <- df$pFDR
      df
    })
    RCPA::plotVennPathway(results_list, pThreshold = params$p_threshold, useFDR = use_fdr_val)
  },

  network = {
    cat(sprintf("%s Reading results from %s\n", ts(), csv_paths[[1]]))
    obj <- read.csv(csv_paths[[1]], check.names = FALSE)
    if (!"p.value" %in% names(obj)) obj$p.value <- obj$pFDR
    top_ids <- if (!is.null(params$top_n_pathways)) {
      head(obj[order(obj$pFDR), "ID"], as.integer(params$top_n_pathways))
    } else NULL
    cat(sprintf("%s Fetching KEGG gene sets for org=%s\n", ts(), params$org))
    genesets <- RCPA::getGeneSets(database = "KEGG", org = params$org)
    RCPA::plotPathwayNetwork(
      list(obj), genesets = genesets, selectedPathways = top_ids,
      pThreshold = params$p_threshold, useFDR = use_fdr_val)
  },

  kegg = {
    cat(sprintf("%s Reading DE results from %s\n", ts(), csv_paths[[1]]))
    rd      <- read.csv(csv_paths[[1]], check.names = FALSE)
    de_list <- list("Study 1" = rd)
    cat(sprintf("%s Generating KEGG map for pathway %s\n", ts(), params$kegg_pathway_id))
    RCPA::plotKEGGMap(de_list,
      KEGGPathwayID = params$kegg_pathway_id,
      useFDR        = use_fdr_val,
      pThreshold    = params$p_threshold)
  },

  ma = {
    cat(sprintf("%s Reading DE results from %s\n", ts(), csv_paths[[1]]))
    rd <- read.csv(csv_paths[[1]], check.names = FALSE)
    RCPA::plotMA(rd,
      pThreshold     = params$p_threshold,
      useFDR         = use_fdr_val,
      logFCThreshold = params$log_fc_threshold)
  },

  stop(paste0("Unhandled plot type: '", params$plot_type, "'"))
)

cat(sprintf("%s Saving %s plot to %s\n", ts(), params$plot_type, params$png_path))
ggplot2::ggsave(params$png_path, plot = p, width = 10, height = 8, dpi = 150, bg = "white")
cat(sprintf("%s Done\n", ts()))
