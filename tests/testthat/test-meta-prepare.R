gs1 <- write_gene_stats_csv(n = 20L)
gs2 <- write_gene_stats_csv(n = 20L)
expr_tmp <- write_expr_csv(n_genes = 10L, n_samples = 6L,
                           sample_names = paste0("s", 1:6))
design_tmp <- write_design_csv(paste0("s", 1:6))
withr::defer(unlink(c(gs1, gs2, expr_tmp, design_tmp)), teardown_env())

cleanup_meta <- function(res) {
  if (is.null(res)) return(invisible())
  for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
  for (j in res$jobs %||% list()) {
    d <- dirname(j$script_path)
    if (dir.exists(d)) unlink(d, recursive = TRUE)
  }
}

test_that("meta_de_prepare rejects fewer than 2 studies", {
  res <- meta_de_prepare(
    list(studies = list(list(expression_uri = file_uri(expr_tmp),
                              experiment_design_uri = file_uri(design_tmp)))),
    de_method = "limma", meta_methods = list("fisher"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "At least 2 studies")
})

test_that("meta_de_prepare rejects invalid de_method", {
  studies <- list(
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp)),
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp))
  )
  res <- meta_de_prepare(list(studies = studies),
                         de_method = "badmethod",
                         meta_methods = list("fisher"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "de_method must be")
})

test_that("meta_de_prepare rejects invalid meta methods", {
  studies <- list(
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp)),
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp))
  )
  res <- meta_de_prepare(list(studies = studies),
                         de_method = "limma",
                         meta_methods = list("wat"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid meta-analysis method")
})

test_that("meta_de_prepare builds jobs for valid input", {
  studies <- list(
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp)),
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp))
  )
  res <- meta_de_prepare(list(studies = studies),
                         de_method = "limma",
                         meta_methods = list("fisher", "stouffer"))
  withr::defer(cleanup_meta(res))
  expect_null(res$error)
  expect_length(res$jobs, 2L)
  for (j in res$jobs) expect_true(file.exists(j$script_path))
})

test_that("meta_pa_prepare rejects fewer than 2 gene_stats_uris", {
  res <- meta_pa_prepare(list(gene_stats_uris = list(file_uri(gs1))),
                         database = "KEGG", pa_method = "ora",
                         org = "hsa", namespace = "biological_process",
                         meta_methods = list("fisher"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "At least 2 gene_stats_uris")
})

test_that("meta_pa_prepare rejects invalid org", {
  res <- meta_pa_prepare(
    list(gene_stats_uris = list(file_uri(gs1), file_uri(gs2))),
    database = "KEGG", pa_method = "ora",
    org = "INVALID", namespace = "biological_process",
    meta_methods = list("fisher"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid KEGG organism")
})

test_that("meta_pa_prepare rejects KEGG-only methods with GO", {
  res <- meta_pa_prepare(
    list(gene_stats_uris = list(file_uri(gs1), file_uri(gs2))),
    database = "GO", pa_method = "spia",
    org = "hsa", namespace = "biological_process",
    meta_methods = list("fisher"))
  cleanup_meta(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "KEGG-only")
})

test_that("meta_pa_prepare builds jobs for valid input", {
  res <- meta_pa_prepare(
    list(gene_stats_uris = list(file_uri(gs1), file_uri(gs2))),
    database = "KEGG", pa_method = "ora",
    org = "hsa", namespace = "biological_process",
    meta_methods = list("fisher", "stouffer", "REML"))
  withr::defer(cleanup_meta(res))
  expect_null(res$error)
  expect_length(res$jobs, 3L)
  for (j in res$jobs) expect_true(file.exists(j$script_path))
})

test_that("meta_build_response surfaces all-failed jobs", {
  jobs <- list(list(success = FALSE, job_name = "j1", stderr = "boom"))
  meta <- list(list(csv_url = "u", csv_fname = "f.csv"))
  res <- meta_build_response(jobs, meta, "de")
  expect_true(is_tool_error(res))
})

test_that("meta_build_response emits links for successful jobs only", {
  jobs <- list(list(success = TRUE, job_name = "j1", stderr = ""),
               list(success = FALSE, job_name = "j2", stderr = "boom"))
  meta <- list(list(csv_url = "u1", csv_fname = "f1.csv"),
               list(csv_url = "u2", csv_fname = "f2.csv"))
  res <- meta_build_response(jobs, meta, "pa")
  expect_false(isTRUE(res$isError))
  types <- vapply(res$content, function(c) c$type, character(1L))
  expect_equal(sum(types == "resource_link"), 1L)
})

test_that("run_meta_handler routes by mode based on input arrays", {
  studies <- list(
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp)),
    list(expression_uri = file_uri(expr_tmp),
         experiment_design_uri = file_uri(design_tmp))
  )
  ctx <- build_mock_ctx(list(
    list(action = "accept", content = list(method = "limma")),
    list(action = "accept", content = list(methods = list("fisher")))
  ))
  args <- elicit_meta_args(list(studies = studies), ctx, "de")
  expect_false(is_tool_error(args))
  expect_equal(args$de_method, "limma")
  expect_setequal(unlist(args$methods), "fisher")
})

test_that("elicit_meta_args returns error when methods missing and no elicitation", {
  ctx <- build_mock_ctx(list())
  ctx$client_capabilities <- list()
  res <- elicit_meta_args(list(gene_stats_uris = list(file_uri(gs1),
                                                       file_uri(gs2)),
                                database = "KEGG", pa_method = "ora",
                                org = "hsa"),
                          ctx, "pa")
  expect_true(is_tool_error(res))
})
