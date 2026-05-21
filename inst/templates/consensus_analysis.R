# Consensus analysis template — sourced by a job script with `params` already set.
# Runs all PA methods in one subprocess, then combines with runConsensusAnalysis.
#
# params:
#   input_type: "gene_stats" | "expression"
#   [gene_stats] gene_stats_path
#   [expression] expr_path, design_json_path, de_method, contrast, has_pairs, pairs
#   database: "KEGG" | "GO"
#   org: KEGG organism code
#   namespace: GO namespace
#   pa_methods: list of PA method names (>= 2)
#   method: "weightedZMean" | "RRA"
#   use_fdr: logical
#   weights: numeric vector or NULL
#   rank_by: "normalizedScore" | "pFDR" | "both"
#   databases_dir: local cache directory for gene set .rda files
#   rds_path, csv_path

library(SummarizedExperiment)
library(RCPA)

ts <- function() format(Sys.time(), "[%H:%M:%S]")

PATHWAY_METHODS <- c("spia", "cepaORA", "cepaGSA")

ORG_TO_TAXID <- list(
  hsa = 9606L, mmu = 10090L, rno = 10116L,
  dme = 7227L, dre = 7955L,  sce = 4932L,
  cel = 6239L, gga = 9031L,  bta = 9913L, ssc = 9823L
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

get_genesets <- function(database, org, namespace, databases_dir) {
  if (identical(database, "KEGG")) {
    cache_name <- paste0("KEGG_", org, ".rda")
    rcpa_args  <- list(database = database, org = org)
  } else {
    taxid      <- ORG_TO_TAXID[[org]] %||% 9606L
    cache_name <- paste0("GO_", taxid, "_", namespace, ".rda")
    rcpa_args  <- list(database = database, taxid = taxid, namespace = namespace)
  }
  cache_path <- file.path(databases_dir, cache_name)
  if (file.exists(cache_path)) {
    cat(sprintf("%s [consensus_analysis] Loading %s gene sets from local cache (%s)...\n",
                ts(), database, cache_name))
    e <- new.env(parent = emptyenv())
    load(cache_path, envir = e)
    return(get(ls(e)[1], envir = e))
  }
  cat(sprintf("%s [consensus_analysis] Downloading %s gene sets (first run — will cache locally)...\n",
              ts(), database))
  gs <- do.call(RCPA::getGeneSets, rcpa_args)
  dir.create(databases_dir, recursive = TRUE, showWarnings = FALSE)
  save(gs, file = cache_path)
  cat(sprintf("%s [consensus_analysis] Cached %s gene sets to %s\n", ts(), database, cache_name))
  gs
}

cat(sprintf("%s [consensus_analysis] Starting: input_type=%s, database=%s, org=%s, method=%s\n",
            ts(), params$input_type, params$database, params$org, params$method))
cat(sprintf("%s [consensus_analysis] PA methods: %s\n",
            ts(), paste(unlist(params$pa_methods), collapse = ", ")))

# ── Build SE from input ───────────────────────────────────────────────────────

if (params$input_type == "gene_stats") {
  cat(sprintf("%s [consensus_analysis] Loading gene stats from %s\n", ts(), params$gene_stats_path))
  gs           <- read.csv(params$gene_stats_path, check.names = FALSE)
  gs$p.value   <- gs$pvalue
  gs$pFDR      <- p.adjust(gs$pvalue, method = "BH")
  gs$statistic <- gs$logFC; gs$avgExpr <- 0; gs$logFCSE <- 1; gs$sampleSize <- 1L
  rd       <- S4Vectors::DataFrame(gs, row.names = gs$id)
  expr_mat <- matrix(gs$logFC, nrow = nrow(gs), ncol = 1, dimnames = list(gs$id, "expr"))
  se       <- SummarizedExperiment(assays = list(expr = expr_mat), rowData = rd)
  cat(sprintf("%s [consensus_analysis] Gene stats SE built: %d genes\n", ts(), nrow(se)))

} else {
  library(jsonlite)
  library(limma)
  cat(sprintf("%s [consensus_analysis] Loading expression matrix from %s\n", ts(), params$expr_path))
  expr_mat    <- as.matrix(read.csv(params$expr_path, row.names = 1, check.names = FALSE))
  design_data <- jsonlite::fromJSON(params$design_json_path)
  col_data    <- S4Vectors::DataFrame(
    group     = factor(design_data$group),
    row.names = design_data$sample
  )
  if (isTRUE(params$has_pairs) && !is.null(params$pairs)) {
    col_data$pair <- factor(unlist(params$pairs))
  }
  common   <- intersect(colnames(expr_mat), rownames(col_data))
  if (length(common) == 0) stop("No matching samples between expression file and experiment_design")
  expr_mat <- expr_mat[, common, drop = FALSE]
  col_data <- col_data[common, , drop = FALSE]
  se_expr  <- SummarizedExperiment(assays = list(counts = expr_mat), colData = col_data)
  id_ann   <- data.frame(FROM = rownames(expr_mat), TO = rownames(expr_mat), stringsAsFactors = FALSE)
  cat(sprintf("%s [consensus_analysis] Expression matrix: %d genes x %d samples\n",
              ts(), nrow(expr_mat), ncol(expr_mat)))

  col_data_df    <- as.data.frame(SummarizedExperiment::colData(se_expr))
  design_formula <- if (isTRUE(params$has_pairs)) ~0 + group + pair else ~0 + group
  design_matrix  <- model.matrix(design_formula, data = col_data_df)
  colnames(design_matrix) <- make.names(colnames(design_matrix))
  contrast_terms  <- trimws(strsplit(params$contrast, "-")[[1]])
  contrast_str_mm <- paste(
    vapply(contrast_terms, function(t) make.names(paste0("group", trimws(t))), character(1)),
    collapse = " - "
  )
  contrast_matrix <- limma::makeContrasts(contrasts = contrast_str_mm, levels = colnames(design_matrix))

  cat(sprintf("%s [consensus_analysis] Running DE analysis (method=%s, contrast='%s')...\n",
              ts(), params$de_method, params$contrast))
  se <- RCPA::runDEAnalysis(
    se_expr, method = params$de_method, design = design_matrix,
    contrast = contrast_matrix, annotation = id_ann
  )
  cat(sprintf("%s [consensus_analysis] DE analysis complete: %d genes\n", ts(), nrow(se)))
}

# ── Preload shared resources ──────────────────────────────────────────────────

pa_methods <- unlist(params$pa_methods)

needs_genesets <- any(!pa_methods %in% PATHWAY_METHODS)
needs_spia     <- "spia" %in% pa_methods
needs_cepa     <- any(c("cepaORA", "cepaGSA") %in% pa_methods)

genesets     <- NULL
network_spia <- NULL
network_cepa <- NULL

if (needs_genesets) {
  genesets <- get_genesets(params$database, params$org, params$namespace %||% "biological_process",
                           params$databases_dir)
}
if (needs_spia) {
  cat(sprintf("%s [consensus_analysis] Loading SPIA KEGG network for %s...\n", ts(), params$org))
  network_spia <- RCPA::getSPIAKEGGNetwork(org = params$org)
}
if (needs_cepa) {
  cat(sprintf("%s [consensus_analysis] Loading CePa pathway catalogue for %s...\n", ts(), params$org))
  network_cepa <- RCPA::getCePaPathwayCatalogue(org = params$org)
}

# ── Run all PA methods ────────────────────────────────────────────────────────

pa_results <- setNames(lapply(pa_methods, function(m) {
  cat(sprintf("%s [consensus_analysis] Running PA method: %s...\n", ts(), m))
  if (m == "spia") {
    RCPA::runPathwayAnalysis(se, network = network_spia, method = m)
  } else if (m %in% c("cepaORA", "cepaGSA")) {
    RCPA::runPathwayAnalysis(se, network = network_cepa, method = m)
  } else {
    RCPA::runGeneSetAnalysis(se, genesets = genesets, method = m)
  }
}), pa_methods)

cat(sprintf("%s [consensus_analysis] All PA methods complete.\n", ts()))

# ── Consensus ─────────────────────────────────────────────────────────────────

weights_val <- if (!is.null(params$weights) && length(params$weights) > 0) {
  as.numeric(unlist(params$weights))
} else NULL

cat(sprintf("%s [consensus_analysis] Running consensus analysis (method=%s, useFDR=%s)...\n",
            ts(), params$method, isTRUE(params$use_fdr)))

result <- RCPA::runConsensusAnalysis(
  pa_results,
  method      = params$method,
  weightsList = weights_val,
  useFDR      = isTRUE(params$use_fdr),
  rank.by     = params$rank_by %||% "normalizedScore"
)

cat(sprintf("%s [consensus_analysis] Consensus complete: %d pathways/gene sets\n", ts(), nrow(result)))

# ── Save results ──────────────────────────────────────────────────────────────

cat(sprintf("%s [consensus_analysis] Saving results to %s\n", ts(), params$csv_path))
saveRDS(result, params$rds_path)
result_sorted <- result[order(result$pFDR), ]
write.csv(result_sorted, params$csv_path, row.names = FALSE)
cat(sprintf("%s [consensus_analysis] Done.\n", ts()))
