# PA meta-analysis template — sourced by a job script with `params` already set.
# Runs PA on each study's gene stats independently, then combines with the specified meta method.
#
# params:
#   gene_stats_paths: list of gene stats CSV paths (one per study)
#   pa_method: single PA method string
#   database: "KEGG" | "GO"
#   org: KEGG organism code
#   namespace: GO namespace
#   meta_method: "fisher" | "stouffer" | "addCLT" | "geoMean" | "minP" | "REML"
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
    cat(sprintf("%s [meta_pa_analysis] Loading %s gene sets from local cache (%s)...\n",
                ts(), database, cache_name))
    e <- new.env(parent = emptyenv())
    load(cache_path, envir = e)
    return(get(ls(e)[1], envir = e))
  }
  cat(sprintf("%s [meta_pa_analysis] Downloading %s gene sets (first run — will cache locally)...\n",
              ts(), database))
  gs <- do.call(RCPA::getGeneSets, rcpa_args)
  dir.create(databases_dir, recursive = TRUE, showWarnings = FALSE)
  save(gs, file = cache_path)
  cat(sprintf("%s [meta_pa_analysis] Cached %s gene sets to %s\n", ts(), database, cache_name))
  gs
}

gs_to_se <- function(gs_path) {
  gs           <- read.csv(gs_path, check.names = FALSE)
  gs$p.value   <- gs$pvalue
  gs$pFDR      <- p.adjust(gs$pvalue, method = "BH")
  gs$statistic <- gs$logFC; gs$avgExpr <- 0; gs$logFCSE <- 1; gs$sampleSize <- 1L
  rd       <- S4Vectors::DataFrame(gs, row.names = gs$id)
  expr_mat <- matrix(gs$logFC, nrow = nrow(gs), ncol = 1, dimnames = list(gs$id, "expr"))
  SummarizedExperiment(assays = list(expr = expr_mat), rowData = rd)
}

cat(sprintf("%s [meta_pa_analysis] Starting: pa_method=%s, database=%s, org=%s, meta_method=%s, n_studies=%d\n",
            ts(), params$pa_method, params$database, params$org, params$meta_method,
            length(params$gene_stats_paths)))

# ── Preload network or gene sets once, shared across all studies ──────────────

if (params$pa_method == "spia") {
  cat(sprintf("%s [meta_pa_analysis] Loading SPIA KEGG network for %s...\n", ts(), params$org))
  network_spia <- RCPA::getSPIAKEGGNetwork(org = params$org)
  run_pa <- function(gs_path, i) {
    cat(sprintf("%s [meta_pa_analysis] Study %d: running SPIA...\n", ts(), i))
    RCPA::runPathwayAnalysis(gs_to_se(gs_path), network = network_spia, method = "spia")
  }
} else if (params$pa_method %in% c("cepaORA", "cepaGSA")) {
  cat(sprintf("%s [meta_pa_analysis] Loading CePa pathway catalogue for %s...\n", ts(), params$org))
  network_cepa <- RCPA::getCePaPathwayCatalogue(org = params$org)
  run_pa <- function(gs_path, i) {
    cat(sprintf("%s [meta_pa_analysis] Study %d: running %s...\n", ts(), i, params$pa_method))
    RCPA::runPathwayAnalysis(gs_to_se(gs_path), network = network_cepa, method = params$pa_method)
  }
} else {
  genesets <- get_genesets(params$database, params$org,
                           params$namespace %||% "biological_process", params$databases_dir)
  run_pa <- function(gs_path, i) {
    cat(sprintf("%s [meta_pa_analysis] Study %d: running %s (%s)...\n",
                ts(), i, params$pa_method, params$database))
    RCPA::runGeneSetAnalysis(gs_to_se(gs_path), genesets = genesets, method = params$pa_method)
  }
}

# ── Run PA for each study ─────────────────────────────────────────────────────

gs_paths   <- unlist(params$gene_stats_paths)
pa_results <- lapply(seq_along(gs_paths), function(i) run_pa(gs_paths[[i]], i))

cat(sprintf("%s [meta_pa_analysis] All studies done. Running meta-analysis (method=%s)...\n",
            ts(), params$meta_method))

result <- RCPA::runPathwayMetaAnalysis(pa_results, method = params$meta_method)

cat(sprintf("%s [meta_pa_analysis] Meta-analysis complete. Saving to %s\n", ts(), params$csv_path))
saveRDS(result, params$rds_path)
if (is.data.frame(result)) {
  result_sorted <- result[order(result$pFDR, na.last = TRUE), ]
} else {
  rd            <- as.data.frame(SummarizedExperiment::rowData(result))
  result_sorted <- rd[order(rd$pFDR, na.last = TRUE), ]
}
write.csv(result_sorted, params$csv_path, row.names = FALSE)
cat(sprintf("%s [meta_pa_analysis] Done.\n", ts()))
