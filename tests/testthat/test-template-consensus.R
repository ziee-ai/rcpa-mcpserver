test_that("consensus template combines ora + fgsea on gene stats", {
  skip_if_no_rcpa()
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_PA_TESTS")),
    "RCPA_RUN_PA_TESTS not set"
  )
  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  gs <- fixture_path("small_gene_stats.csv")

  csv_path <- file.path(out_dir, "consensus.csv")
  rds_path <- file.path(out_dir, "consensus.rds")
  res <- run_template("consensus_analysis", list(
    input_type = "gene_stats",
    gene_stats_path = gs,
    database = "KEGG",
    org = "hsa",
    namespace = "biological_process",
    pa_methods = list("ora", "fgsea"),
    method = "weightedZMean",
    use_fdr = TRUE,
    weights = NULL,
    rank_by = "normalizedScore",
    databases_dir = cache_dir,
    rds_path = rds_path,
    csv_path = csv_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(file.exists(csv_path))
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_gt(nrow(df), 0L)
})

test_that("consensus template runs with RRA method", {
  skip_if_no_rcpa()
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_PA_TESTS")),
    "RCPA_RUN_PA_TESTS not set"
  )
  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  gs <- fixture_path("small_gene_stats.csv")

  csv_path <- file.path(out_dir, "consensus_rra.csv")
  rds_path <- file.path(out_dir, "consensus_rra.rds")
  res <- run_template("consensus_analysis", list(
    input_type = "gene_stats",
    gene_stats_path = gs,
    database = "KEGG",
    org = "hsa",
    namespace = "biological_process",
    pa_methods = list("ora", "fgsea"),
    method = "RRA",
    use_fdr = TRUE,
    weights = NULL,
    rank_by = "normalizedScore",
    databases_dir = cache_dir,
    rds_path = rds_path,
    csv_path = csv_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
})
