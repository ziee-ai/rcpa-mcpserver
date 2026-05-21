gs_tmp <- write_gene_stats_csv(n = 30L)
gs_uri <- file_uri(gs_tmp)
withr::defer(unlink(gs_tmp), teardown_env())

cleanup_consensus <- function(res) {
  if (is.null(res)) return(invisible())
  for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
  if (!is.null(res$script_path)) {
    unlink(dirname(res$script_path), recursive = TRUE)
  }
}

test_that("consensus_prepare rejects invalid org", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "KEGG",
                           pa_methods = list("ora", "fgsea"),
                           org = "INVALID", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid KEGG organism")
})

test_that("consensus_prepare rejects invalid database", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "Reactome",
                           pa_methods = list("ora", "fgsea"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid database")
})

test_that("consensus_prepare requires at least 2 pa_methods", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "KEGG",
                           pa_methods = list("ora"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "At least 2 PA methods")
})

test_that("consensus_prepare rejects invalid consensus method", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "KEGG",
                           pa_methods = list("ora", "fgsea"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "badmethod",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid consensus method")
})

test_that("consensus_prepare rejects KEGG-only methods with GO database", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "GO",
                           pa_methods = list("spia", "cepaORA"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "KEGG-only")
})

test_that("consensus_prepare rejects weights with wrong length", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "KEGG",
                           pa_methods = list("ora", "fgsea", "ks"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = list(0.5, 1, 2, 3),
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "weights length")
})

test_that("consensus_prepare requires an input mode", {
  res <- consensus_prepare(list(),
                           database = "KEGG",
                           pa_methods = list("ora", "fgsea"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  cleanup_consensus(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "gene_stats_uri or expression_uri")
})

test_that("consensus_prepare builds a job script for valid args", {
  res <- consensus_prepare(list(gene_stats_uri = gs_uri),
                           database = "KEGG",
                           pa_methods = list("ora", "fgsea"),
                           org = "hsa", namespace = "biological_process",
                           de_method = "limma", method = "weightedZMean",
                           use_fdr = TRUE, weights = NULL,
                           rank_by = "normalizedScore")
  withr::defer(cleanup_consensus(res))
  expect_null(res$error)
  expect_true(file.exists(res$script_path))
  expect_equal(res$method, "weightedZMean")
  expect_setequal(res$pa_methods, c("ora", "fgsea"))
  params <- jsonlite::fromJSON(
    file.path(dirname(res$script_path),
              paste0(res$job_name, "_params.json")),
    simplifyVector = FALSE)
  expect_equal(params$method, "weightedZMean")
  expect_equal(params$rank_by, "normalizedScore")
  expect_true(isTRUE(params$use_fdr))
})

test_that("consensus_build_response surfaces job failure", {
  res <- consensus_build_response(
    list(success = FALSE, stderr = "boom",
         stdout = "", exit_code = 1L),
    list(csv_path = tempfile(), csv_url = "http://x",
         csv_fname = "x.csv", method = "RRA",
         pa_methods = c("ora", "fgsea"), run_id = "test"))
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "boom")
})
