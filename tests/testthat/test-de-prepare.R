sample_names <- paste0("sample", 1:10)
expr_tmp <- write_expr_csv(n_genes = 20L, n_samples = 10L)
design_tmp <- write_design_csv(sample_names)
expr_uri   <- file_uri(expr_tmp)
design_uri <- file_uri(design_tmp)

withr::defer(unlink(c(expr_tmp, design_tmp)), teardown_env())

test_that("unsupported gene_id_type returns error", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri,
    gene_id_type = "symbol"
  ))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "[Ee]ntrez")
})

test_that("missing expression_uri returns error", {
  res <- de_analysis_prepare(list(expression_uri = ""))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "expression_uri")
})

test_that("file:// URI rejected by safety check when not whitelisted", {
  withr::with_envvar(c("RCPA_ALLOW_LOCAL_URIS" = "FALSE"), {
    res <- de_analysis_prepare(list(
      expression_uri = "file:///etc/passwd",
      experiment_design_uri = design_uri
    ))
    expect_false(is.null(res$error))
    expect_match(error_text(res$error), "Failed to fetch")
  })
})

test_that("invalid method returns error", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri,
    method = "badmethod"
  ))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "limma|DESeq2|edgeR")
})

test_that("more than 2 groups without contrast returns error", {
  tmp_d <- tempfile(fileext = ".csv")
  withr::defer(unlink(tmp_d))
  df <- data.frame(sample = paste0("s", 1:6),
                   group  = c("A", "A", "B", "B", "C", "C"))
  utils::write.csv(df, tmp_d, row.names = FALSE)
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = file_uri(tmp_d)
  ))
  expect_false(is.null(res$error))
  txt <- error_text(res$error)
  expect_match(txt, "contrast")
  expect_match(txt, "A|B|C")
})

test_that("2-group design auto-infers contrast", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri
  ))
  withr::defer({
    for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
    if (!is.null(res$script_path)) unlink(dirname(res$script_path),
                                          recursive = TRUE)
  })
  expect_null(res$error)
  expect_match(res$contrast, "Treatment\\s*-\\s*Control")
})

test_that("contrast referencing unknown group rejected", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri,
    method = "limma",
    contrast = "Foo - Bar"
  ))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "unknown group")
})

test_that("full valid args produce a job script with absolute paths", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri,
    method = "limma",
    contrast = "Treatment - Control"
  ))
  withr::defer({
    for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
    if (!is.null(res$script_path)) unlink(dirname(res$script_path),
                                          recursive = TRUE)
  })
  expect_null(res$error)
  expect_true(file.exists(res$script_path))
  expect_true(nzchar(res$csv_path))
  expect_true(nzchar(res$csv_url))
  expect_equal(res$method, "limma")
  expect_equal(res$contrast, "Treatment - Control")
  expect_length(res$tmp_files, 3L)
  expect_match(res$csv_url, "^http")
  expect_match(res$csv_url, "results/")
})

test_that("paired design is detected", {
  paired_design <- write_design_csv(sample_names,
                                    pairs = rep(1:5, times = 2L))
  withr::defer(unlink(paired_design))
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = file_uri(paired_design)
  ))
  withr::defer({
    for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
    if (!is.null(res$script_path)) unlink(dirname(res$script_path),
                                          recursive = TRUE)
  })
  expect_null(res$error)
  params <- jsonlite::fromJSON(
    file.path(dirname(res$script_path),
              paste0(res$job_name, "_params.json")),
    simplifyVector = FALSE)
  expect_true(isTRUE(params$has_pairs))
  expect_length(params$pairs, 10L)
})

test_that("contrast with extra whitespace is canonicalised", {
  res <- de_analysis_prepare(list(
    expression_uri = expr_uri,
    experiment_design_uri = design_uri,
    method = "limma",
    contrast = "  Treatment   -   Control  "
  ))
  withr::defer({
    for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
    if (!is.null(res$script_path)) unlink(dirname(res$script_path),
                                          recursive = TRUE)
  })
  expect_null(res$error)
})

test_that("de_build_response surfaces subprocess failure", {
  bad_job <- list(success = FALSE, stderr = "boom",
                  stdout = "", exit_code = 1L)
  prep <- list(csv_path = tempfile(),
               csv_url = "http://x/y.csv",
               csv_filename = "y.csv",
               method = "limma", contrast = "A - B",
               run_id = "test")
  res <- de_build_response(bad_job, prep)
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "boom")
})

test_that("de_build_response counts genes and significant from CSV", {
  csv <- tempfile(fileext = ".csv")
  withr::defer(unlink(csv))
  df <- data.frame(ID = paste0("g", 1:5),
                   logFC = stats::rnorm(5L),
                   p.value = c(0.001, 0.5, 0.04, 0.06, 0.03),
                   pFDR = c(0.005, 0.6, 0.04, 0.1, 0.02))
  utils::write.csv(df, csv, row.names = FALSE)
  job <- list(success = TRUE, stderr = "", stdout = "", exit_code = 0L)
  prep <- list(csv_path = csv,
               csv_url = "http://x/y.csv",
               csv_filename = "y.csv",
               method = "limma", contrast = "A - B",
               run_id = "test")
  res <- de_build_response(job, prep)
  expect_false(isTRUE(res$isError))
  meta <- jsonlite::fromJSON(res$content[[1L]]$text, simplifyVector = TRUE)
  expect_equal(meta$n_genes, 5L)
  expect_equal(meta$n_significant, 3L)
  expect_equal(meta$result_type, "de_result")
})
