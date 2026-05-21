# DE meta-analysis template — sourced by a job script with `params` already set.
# Runs DE on each study independently, then combines with the specified meta-analysis method.
#
# params:
#   studies: list of list(expr_path, design_path, contrast, has_pairs, pairs)
#   de_method: "limma" | "DESeq2" | "edgeR"
#   meta_method: "fisher" | "stouffer" | "addCLT" | "geoMean" | "minP" | "REML"
#   rds_path, csv_path

library(SummarizedExperiment)
library(jsonlite)
library(limma)
library(RCPA)

ts <- function() format(Sys.time(), "[%H:%M:%S]")

cat(sprintf("%s [meta_de_analysis] Starting: de_method=%s, meta_method=%s, n_studies=%d\n",
            ts(), params$de_method, params$meta_method, length(params$studies)))

run_de_for_study <- function(study, i) {
  cat(sprintf("%s [meta_de_analysis] Study %d: loading expression matrix from %s\n",
              ts(), i, study$expr_path))
  expr_mat    <- as.matrix(read.csv(study$expr_path, row.names = 1, check.names = FALSE))
  design_data <- jsonlite::fromJSON(study$design_path)
  col_data    <- S4Vectors::DataFrame(
    group     = factor(design_data$group),
    row.names = design_data$sample
  )
  if (isTRUE(study$has_pairs) && !is.null(study$pairs)) {
    col_data$pair <- factor(unlist(study$pairs))
  }
  common   <- intersect(colnames(expr_mat), rownames(col_data))
  if (length(common) == 0) stop(paste("Study", i, ": no matching samples between expression file and design"))
  expr_mat <- expr_mat[, common, drop = FALSE]
  col_data <- col_data[common, , drop = FALSE]
  cat(sprintf("%s [meta_de_analysis] Study %d: %d genes x %d samples\n",
              ts(), i, nrow(expr_mat), length(common)))

  se     <- SummarizedExperiment(assays = list(counts = expr_mat), colData = col_data)
  id_ann <- data.frame(FROM = rownames(expr_mat), TO = rownames(expr_mat), stringsAsFactors = FALSE)

  col_data_df    <- as.data.frame(SummarizedExperiment::colData(se))
  design_formula <- if (isTRUE(study$has_pairs)) ~0 + group + pair else ~0 + group
  design_matrix  <- model.matrix(design_formula, data = col_data_df)
  colnames(design_matrix) <- make.names(colnames(design_matrix))
  contrast_terms  <- trimws(strsplit(study$contrast, "-")[[1]])
  contrast_str_mm <- paste(
    vapply(contrast_terms, function(t) make.names(paste0("group", trimws(t))), character(1)),
    collapse = " - "
  )
  contrast_matrix <- limma::makeContrasts(contrasts = contrast_str_mm, levels = colnames(design_matrix))

  cat(sprintf("%s [meta_de_analysis] Study %d: running DE (method=%s, contrast='%s')...\n",
              ts(), i, params$de_method, study$contrast))
  result <- RCPA::runDEAnalysis(
    se, method = params$de_method, design = design_matrix,
    contrast = contrast_matrix, annotation = id_ann
  )
  cat(sprintf("%s [meta_de_analysis] Study %d: DE complete: %d genes\n", ts(), i, nrow(result)))
  result
}

de_results_se <- lapply(seq_along(params$studies), function(i) {
  run_de_for_study(params$studies[[i]], i)
})

# RCPA::runDEMetaAnalysis expects a list of data frames with columns
# {ID, p.value, logFC, logFCSE, sampleSize}. runDEAnalysis returns a
# SummarizedExperiment with those columns in rowData, so extract first.
de_results <- lapply(de_results_se, function(r) {
  if (is.data.frame(r)) return(r)
  rd <- as.data.frame(SummarizedExperiment::rowData(r))
  if (!"ID" %in% colnames(rd) && !is.null(rownames(rd))) {
    rd$ID <- rownames(rd)
  }
  rd
})

cat(sprintf("%s [meta_de_analysis] All studies done. Running meta-analysis (method=%s)...\n",
            ts(), params$meta_method))

result <- RCPA::runDEMetaAnalysis(de_results, method = params$meta_method)

cat(sprintf("%s [meta_de_analysis] Meta-analysis complete. Saving to %s\n", ts(), params$csv_path))
saveRDS(result, params$rds_path)
if (is.data.frame(result)) {
  result_sorted <- result[order(result$pFDR, na.last = TRUE), ]
} else {
  rd            <- as.data.frame(SummarizedExperiment::rowData(result))
  result_sorted <- rd[order(rd$pFDR, na.last = TRUE), ]
}
write.csv(result_sorted, params$csv_path, row.names = FALSE)
cat(sprintf("%s [meta_de_analysis] Done.\n", ts()))
