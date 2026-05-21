# Pathway-analysis template smoke tests.
#
# These exercise the gene-set methods (ora/fgsea/ks/wilcox) which need
# RCPA gene-set catalogs but NOT the KEGG topology network. The topology
# methods (spia/cepaORA/cepaGSA) are gated behind RCPA_RUN_SPIA_TESTS
# because they download network data on first use.

skip_if_pa_offline <- function() {
  # The first call to RCPA::getGeneSets("KEGG", org="hsa") may hit the
  # network for a fresh catalogue. We skip these tests by default
  # outside CI environments that have RCPA_RUN_TEMPLATE_TESTS set.
  skip_if_no_rcpa()
  # An additional opt-in for tests that need actual KEGG data.
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_PA_TESTS")),
    "RCPA_RUN_PA_TESTS not set"
  )
}

test_that("PA template (ora + KEGG + hsa) produces a pathway CSV", {
  skip_if_pa_offline()
  gs <- fixture_path("small_gene_stats.csv")

  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  csv_path <- file.path(out_dir, "pa_ora.csv")
  rds_path <- file.path(out_dir, "pa_ora.rds")
  res <- run_template("pathway_analysis", list(
    input_type = "gene_stats",
    gene_stats_path = gs,
    method = "ora",
    database = "KEGG",
    org = "hsa",
    namespace = "biological_process",
    databases_dir = cache_dir,
    rds_path = rds_path,
    csv_path = csv_path
  ))

  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(file.exists(csv_path))
  pa <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  for (col in c("ID", "p.value", "pFDR")) {
    expect_true(col %in% colnames(pa), info = col)
  }
  expect_gt(nrow(pa), 0L)
})

test_that("PA template (fgsea + KEGG + hsa) produces a pathway CSV", {
  skip_if_pa_offline()
  gs <- fixture_path("small_gene_stats.csv")

  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  csv_path <- file.path(out_dir, "pa_fgsea.csv")
  rds_path <- file.path(out_dir, "pa_fgsea.rds")
  res <- run_template("pathway_analysis", list(
    input_type = "gene_stats",
    gene_stats_path = gs,
    method = "fgsea",
    database = "KEGG",
    org = "hsa",
    namespace = "biological_process",
    databases_dir = cache_dir,
    rds_path = rds_path,
    csv_path = csv_path
  ))

  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(file.exists(csv_path))
})
