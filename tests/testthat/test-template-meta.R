# Meta-analysis templates: DE and PA modes.

test_that("meta_de template combines 2 studies with limma + fisher", {
  skip_if_no_rcpa()
  out_dir <- withr::local_tempdir()
  expr <- fixture_path("small_expr.csv")
  design <- write_design_json(paste0("sample", 1:10))
  withr::defer(unlink(design))

  studies <- list(
    list(expr_path = expr, design_path = design,
         contrast = "Treatment - Control",
         has_pairs = FALSE, pairs = NULL),
    list(expr_path = expr, design_path = design,
         contrast = "Treatment - Control",
         has_pairs = FALSE, pairs = NULL)
  )

  csv_path <- file.path(out_dir, "meta_de.csv")
  rds_path <- file.path(out_dir, "meta_de.rds")
  res <- run_template("meta_de_analysis", list(
    studies = studies,
    de_method = "limma",
    meta_method = "fisher",
    rds_path = rds_path,
    csv_path = csv_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(file.exists(csv_path))
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_gt(nrow(df), 0L)
  for (col in c("ID", "p.value", "pFDR")) {
    expect_true(col %in% colnames(df), info = col)
  }
})

test_that("meta_pa template combines 2 gene-stats studies (ora + KEGG)", {
  skip_if_no_rcpa()
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_PA_TESTS")),
    "RCPA_RUN_PA_TESTS not set"
  )
  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  gs1 <- fixture_path("small_gene_stats.csv")
  gs2 <- fixture_path("small_gene_stats_alt.csv")

  csv_path <- file.path(out_dir, "meta_pa.csv")
  rds_path <- file.path(out_dir, "meta_pa.rds")
  res <- run_template("meta_pa_analysis", list(
    gene_stats_paths = list(gs1, gs2),
    pa_method = "ora",
    database = "KEGG",
    org = "hsa",
    namespace = "biological_process",
    meta_method = "fisher",
    databases_dir = cache_dir,
    rds_path = rds_path,
    csv_path = csv_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(file.exists(csv_path))
})
