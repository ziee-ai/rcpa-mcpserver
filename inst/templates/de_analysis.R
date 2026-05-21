# DE analysis template — sourced by a job script with `params` already set.
# params: expr_path, design_json_path, method, contrast,
#         has_pairs (logical), pairs (integer vector or NULL),
#         rds_path, csv_path

library(SummarizedExperiment)
library(jsonlite)
library(limma)
library(RCPA)

ts <- function() format(Sys.time(), "[%H:%M:%S]")

cat(sprintf("%s Loading expression matrix from %s\n", ts(), params$expr_path))
expr_mat <- tryCatch(
  as.matrix(read.csv(params$expr_path, row.names = 1, check.names = FALSE)),
  error = function(e) stop(paste("Failed to read expression file:", e$message))
)
cat(sprintf("%s Expression matrix: %d genes x %d samples\n", ts(), nrow(expr_mat), ncol(expr_mat)))

cat(sprintf("%s Loading experiment design...\n", ts()))
design_data <- jsonlite::fromJSON(params$design_json_path)
col_data <- S4Vectors::DataFrame(
  group     = factor(design_data$group),
  row.names = design_data$sample
)
if (isTRUE(params$has_pairs) && !is.null(params$pairs)) {
  col_data$pair <- factor(unlist(params$pairs))
}

common <- intersect(colnames(expr_mat), rownames(col_data))
if (length(common) == 0) stop("No matching samples between expression file and experiment_design")
expr_mat <- expr_mat[, common, drop = FALSE]
col_data  <- col_data[common, , drop = FALSE]
cat(sprintf("%s Matched %d samples\n", ts(), length(common)))

se            <- SummarizedExperiment(assays = list(counts = expr_mat), colData = col_data)
gene_ids      <- rownames(expr_mat)
id_annotation <- data.frame(FROM = gene_ids, TO = gene_ids, stringsAsFactors = FALSE)

col_data_df    <- as.data.frame(SummarizedExperiment::colData(se))
design_formula <- if (isTRUE(params$has_pairs)) ~0 + group + pair else ~0 + group
design_matrix  <- model.matrix(design_formula, data = col_data_df)
colnames(design_matrix) <- make.names(colnames(design_matrix))

contrast_terms  <- trimws(strsplit(params$contrast, "-")[[1]])
contrast_str_mm <- paste(
  vapply(contrast_terms, function(t) make.names(paste0("group", trimws(t))), character(1)),
  collapse = " - "
)
contrast_matrix <- limma::makeContrasts(contrasts = contrast_str_mm, levels = colnames(design_matrix))

cat(sprintf("%s Running %s DE analysis for contrast: %s\n", ts(), params$method, params$contrast))
result <- RCPA::runDEAnalysis(
  se,
  method     = params$method,
  design     = design_matrix,
  contrast   = contrast_matrix,
  annotation = id_annotation
)

cat(sprintf("%s Saving results to %s\n", ts(), params$csv_path))
saveRDS(result, params$rds_path)
rd        <- as.data.frame(SummarizedExperiment::rowData(result))
rd_sorted <- rd[order(rd$pFDR), ]
write.csv(rd_sorted, params$csv_path, row.names = FALSE)
cat(sprintf("%s DE analysis complete: %d genes\n", ts(), nrow(rd_sorted)))
