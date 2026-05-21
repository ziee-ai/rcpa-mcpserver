# Generate plots from a small DE CSV produced by the limma path.

prepare_de_csv <- function() {
  # Run the DE template once and reuse its output across plot tests
  # within this file. testthat 3 runs each test_that() in its own
  # call frame, so we re-generate per test for isolation.
  expr <- fixture_path("small_expr.csv")
  design_json <- write_design_json(paste0("sample", 1:10))
  out_dir <- withr::local_tempdir(.local_envir = parent.frame())
  csv_path <- file.path(out_dir, "de.csv")
  rds_path <- file.path(out_dir, "de.rds")
  withr::defer(unlink(design_json), envir = parent.frame())
  res <- run_template("de_analysis", list(
    expr_path = expr,
    design_json_path = design_json,
    method = "limma",
    contrast = "Treatment - Control",
    has_pairs = FALSE,
    pairs = NULL,
    rds_path = rds_path,
    csv_path = csv_path
  ))
  if (!res$result$success) {
    skip(paste("DE prep failed:", res$result$stderr))
  }
  csv_path
}

test_that("plot template renders a valid volcano PNG from a DE CSV", {
  skip_if_no_rcpa()
  csv_path <- prepare_de_csv()
  out_dir <- withr::local_tempdir()
  png_path <- file.path(out_dir, "volcano.png")
  res <- run_template("plot_results", list(
    plot_type = "volcano",
    result_type = "de_result",
    csv_paths = list(csv_path),
    is_de = TRUE,
    p_threshold = 0.05,
    use_fdr = TRUE,
    log_fc_threshold = 1,
    kegg_pathway_id = "",
    org = "hsa",
    top_n_genes = 50L,
    top_n_pathways = NULL,
    png_path = png_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(is_valid_png(png_path))
})

test_that("plot template renders MA plot from a DE CSV", {
  skip_if_no_rcpa()
  csv_path <- prepare_de_csv()
  out_dir <- withr::local_tempdir()
  png_path <- file.path(out_dir, "ma.png")
  res <- run_template("plot_results", list(
    plot_type = "ma",
    result_type = "de_result",
    csv_paths = list(csv_path),
    is_de = TRUE,
    p_threshold = 0.05,
    use_fdr = TRUE,
    log_fc_threshold = 1,
    kegg_pathway_id = "",
    org = "hsa",
    top_n_genes = 50L,
    top_n_pathways = NULL,
    png_path = png_path
  ))
  expect_true(res$result$success,
              info = paste("stderr:", res$result$stderr))
  expect_true(is_valid_png(png_path))
})
