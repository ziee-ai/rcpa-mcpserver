# Pathway analysis template — sourced by a job script with `params` already set.
# One job = one (method × database) combination.
#
# params:
#   input_type: "gene_stats" | "gene_list" | "expression"
#   method:     ora | fgsea | gsa | ks | wilcox | spia | cepaORA | cepaGSA
#   database:   "KEGG" | "GO"
#   org:        KEGG organism code (e.g. "hsa")
#   namespace:  GO namespace (e.g. "biological_process")
#
#   [gene_stats]  gene_stats_path
#   [gene_list]   gene_ids (list), background_gene_ids (list or NULL)
#   [expression]  expr_path, design_json_path, de_method, contrast,
#                 has_pairs (logical), pairs (integer vector or NULL)
#
#   databases_dir: local cache directory for gene set .rda files
#   rds_path, csv_path

library(SummarizedExperiment)
library(RCPA)

ts <- function() format(Sys.time(), "[%H:%M:%S]")

PATHWAY_METHODS <- c("spia", "cepaORA", "cepaGSA")

cat(sprintf("%s [pathway_analysis] Starting: method=%s db=%s org=%s input_type=%s\n",
            ts(), params$method, params$database, params$org, params$input_type))

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
    cat(sprintf("%s [pathway_analysis] Loading %s gene sets from local cache (%s)...\n",
                ts(), database, cache_name))
    e <- new.env(parent = emptyenv())
    load(cache_path, envir = e)
    return(get(ls(e)[1], envir = e))
  }

  cat(sprintf("%s [pathway_analysis] Downloading %s gene sets (first run — will cache locally)...\n",
              ts(), database))
  gs <- do.call(RCPA::getGeneSets, rcpa_args)
  dir.create(databases_dir, recursive = TRUE, showWarnings = FALSE)
  save(gs, file = cache_path)
  cat(sprintf("%s [pathway_analysis] Cached %s gene sets to %s\n", ts(), database, cache_name))
  gs
}

build_gene_list_se <- function(gene_ids, bg_only) {
  all_genes <- c(gene_ids, bg_only)
  n_fg <- length(gene_ids); n_bg <- length(bg_only)
  rd <- S4Vectors::DataFrame(
    ID        = all_genes,
    logFC     = c(rep(1,     n_fg), rep(0,   n_bg)),
    p.value   = c(rep(0.001, n_fg), rep(1.0, n_bg)),
    pFDR      = c(rep(0.001, n_fg), rep(1.0, n_bg)),
    statistic = c(rep(1,     n_fg), rep(0,   n_bg)),
    avgExpr   = rep(0, length(all_genes)),
    logFCSE   = rep(1, length(all_genes)),
    sampleSize = rep(1L, length(all_genes)),
    row.names = all_genes
  )
  expr_mat <- matrix(
    c(rep(1, n_fg), rep(0, n_bg)), nrow = length(all_genes), ncol = 1,
    dimnames = list(all_genes, "expr")
  )
  SummarizedExperiment(assays = list(expr = expr_mat), rowData = rd)
}

# ── Build SummarizedExperiment ────────────────────────────────────────────────

if (params$input_type == "gene_stats") {
  cat(sprintf("%s [pathway_analysis] Loading gene stats from %s\n", ts(), params$gene_stats_path))
  gs         <- read.csv(params$gene_stats_path, check.names = FALSE)
  gs$p.value   <- gs$pvalue
  gs$pFDR      <- p.adjust(gs$pvalue, method = "BH")
  gs$statistic <- gs$logFC
  gs$avgExpr   <- 0
  gs$logFCSE   <- 1
  gs$sampleSize <- 1L
  rd       <- S4Vectors::DataFrame(gs, row.names = gs$id)
  expr_mat <- matrix(gs$logFC, nrow = nrow(gs), ncol = 1, dimnames = list(gs$id, "expr"))
  se       <- SummarizedExperiment(assays = list(expr = expr_mat), rowData = rd)
  cat(sprintf("%s [pathway_analysis] Gene stats SE built: %d genes\n", ts(), nrow(se)))

} else if (params$input_type == "gene_list") {
  library(jsonlite)
  gene_ids <- unlist(params$gene_ids)
  cat(sprintf("%s [pathway_analysis] Gene list mode: %d foreground genes\n", ts(), length(gene_ids)))

  if (params$method %in% PATHWAY_METHODS) {
    cat(sprintf("%s [pathway_analysis] Loading KEGG gene sets for background derivation...\n", ts()))
    bg_genesets <- get_genesets("KEGG", params$org, NULL, params$databases_dir)
    bg_only     <- setdiff(unique(unlist(bg_genesets$genesets)), gene_ids)
    se          <- build_gene_list_se(gene_ids, bg_only)
  } else if (!is.null(params$background_gene_ids) && length(params$background_gene_ids) > 0) {
    bg_only <- setdiff(unlist(params$background_gene_ids), gene_ids)
    cat(sprintf("%s [pathway_analysis] Using provided background: %d genes\n", ts(), length(bg_only)))
    se <- build_gene_list_se(gene_ids, bg_only)
  } else {
    cat(sprintf("%s [pathway_analysis] Loading %s gene sets for background derivation...\n",
                ts(), params$database))
    genesets <- get_genesets(params$database, params$org, params$namespace, params$databases_dir)
    bg_only  <- setdiff(unique(unlist(genesets$genesets)), gene_ids)
    se       <- build_gene_list_se(gene_ids, bg_only)
  }
  cat(sprintf("%s [pathway_analysis] Gene list SE built: %d total genes (fg + bg)\n", ts(), nrow(se)))

} else if (params$input_type == "expression") {
  library(jsonlite)
  library(limma)
  cat(sprintf("%s [pathway_analysis] Loading expression matrix from %s\n", ts(), params$expr_path))
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

  cat(sprintf("%s [pathway_analysis] Running DE analysis (method=%s, contrast='%s')...\n",
              ts(), params$de_method, params$contrast))
  se <- RCPA::runDEAnalysis(
    se_expr, method = params$de_method,
    design = design_matrix, contrast = contrast_matrix,
    annotation = id_ann
  )
  cat(sprintf("%s [pathway_analysis] DE analysis complete: %d genes\n", ts(), nrow(se)))
}

# ── Load gene sets or pathway network & run analysis ─────────────────────────

if (params$method %in% PATHWAY_METHODS) {
  if (params$method == "spia") {
    cat(sprintf("%s [pathway_analysis] Loading SPIA KEGG network for %s...\n", ts(), params$org))
    network <- RCPA::getSPIAKEGGNetwork(org = params$org)
  } else {
    cat(sprintf("%s [pathway_analysis] Loading CePa pathway catalogue for %s...\n", ts(), params$org))
    network <- RCPA::getCePaPathwayCatalogue(org = params$org)
  }
  cat(sprintf("%s [pathway_analysis] Running %s pathway analysis...\n", ts(), params$method))
  result <- RCPA::runPathwayAnalysis(se, network = network, method = params$method)
} else {
  if (!exists("genesets")) {
    genesets <- get_genesets(params$database, params$org, params$namespace, params$databases_dir)
  }
  cat(sprintf("%s [pathway_analysis] Running %s gene set analysis (db=%s)...\n",
              ts(), params$method, params$database))
  result <- RCPA::runGeneSetAnalysis(se, genesets = genesets, method = params$method)
}

cat(sprintf("%s [pathway_analysis] Analysis complete: %d pathways/gene sets\n", ts(), nrow(result)))

# ── Save results ──────────────────────────────────────────────────────────────

cat(sprintf("%s [pathway_analysis] Saving results to %s\n", ts(), params$csv_path))
saveRDS(result, params$rds_path)
result_sorted <- result[order(result$pFDR), ]
write.csv(result_sorted, params$csv_path, row.names = FALSE)

cat(sprintf("%s [pathway_analysis] Done.\n", ts()))
