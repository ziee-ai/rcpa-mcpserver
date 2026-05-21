gene_stats_tmp <- write_gene_stats_csv(n = 30L)
gene_stats_uri <- file_uri(gene_stats_tmp)
expr_tmp <- write_expr_csv(n_genes = 20L, n_samples = 10L)
expr_uri <- file_uri(expr_tmp)
design_tmp <- write_design_csv(paste0("sample", 1:10))
design_uri <- file_uri(design_tmp)

withr::defer(unlink(c(gene_stats_tmp, expr_tmp, design_tmp)),
             teardown_env())

cleanup_prep <- function(res) {
  if (is.null(res)) return(invisible())
  for (p in res$tmp_files) if (!is.null(p) && file.exists(p)) unlink(p)
  for (c in res$combos %||% list()) {
    d <- dirname(c$script_path)
    if (dir.exists(d)) unlink(d, recursive = TRUE)
  }
}

test_that("validate_org accepts well-formed codes", {
  expect_true(validate_org("hsa"))
  expect_true(validate_org("mmu"))
  expect_true(validate_org("rno"))
  expect_true(validate_org("dmel"))
})

test_that("validate_org rejects malformed codes", {
  expect_false(validate_org("HSA"))
  expect_false(validate_org("h"))
  expect_false(validate_org("homo_sapiens"))
  expect_false(validate_org("123"))
})

test_that("pa_prepare rejects invalid org", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("KEGG"), methods = list("ora"),
    org = "HUMAN", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid KEGG organism")
})

test_that("pa_prepare rejects unknown methods", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("KEGG"), methods = list("doesnotexist"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid method")
})

test_that("pa_prepare rejects unknown databases", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("Reactome"), methods = list("ora"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid database")
})

test_that("pa_prepare rejects invalid namespace", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("GO"), methods = list("ora"),
    org = "hsa", namespace = "not_a_namespace"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Invalid GO namespace")
})

test_that("pa_prepare requires at least one input mode", {
  res <- pa_prepare(
    list(),
    databases = list("KEGG"), methods = list("ora"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "gene_stats_uri, gene_list, or expression_uri")
})

test_that("pa_prepare rejects gsa with gene_stats input", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("KEGG"), methods = list("gsa"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "gsa.*requires raw expression")
})

test_that("pa_prepare rejects non-ORA methods with gene_list", {
  res <- pa_prepare(
    list(gene_list = list("gene1", "gene2", "gene3")),
    databases = list("KEGG"), methods = list("fgsea"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Gene list input only supports")
})

test_that("pa_prepare rejects non-gsa methods with expression input", {
  res <- pa_prepare(
    list(expression_uri = expr_uri,
         experiment_design_uri = design_uri),
    databases = list("KEGG"), methods = list("ora"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Expression input only supports")
})

test_that("pa_prepare rejects expression input without design_uri", {
  res <- pa_prepare(
    list(expression_uri = expr_uri),
    databases = list("KEGG"), methods = list("gsa"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "experiment_design_uri")
})

test_that("pa_prepare builds combos for gene_stats × KEGG × {ora,fgsea}", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("KEGG"), methods = list("ora", "fgsea"),
    org = "hsa", namespace = "biological_process"
  )
  withr::defer(cleanup_prep(res))
  expect_null(res$error)
  expect_length(res$combos, 2L)
  expect_setequal(vapply(res$combos, `[[`, character(1L), "method"),
                  c("ora", "fgsea"))
  for (c in res$combos) expect_true(file.exists(c$script_path))
})

test_that("pa_prepare skips KEGG-only methods for GO", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("GO"), methods = list("fgsea", "spia", "cepaORA"),
    org = "hsa", namespace = "biological_process"
  )
  withr::defer(cleanup_prep(res))
  expect_null(res$error)
  expect_length(res$combos, 1L)
  expect_equal(res$combos[[1L]]$method, "fgsea")
  expect_setequal(res$skipped_kegg_only, c("spia", "cepaORA"))
})

test_that("pa_prepare returns error when all jobs would be KEGG-only on GO", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("GO"), methods = list("spia"),
    org = "hsa", namespace = "biological_process"
  )
  cleanup_prep(res)
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "KEGG-only")
})

test_that("pa_prepare expands cartesian product of dbs × methods", {
  res <- pa_prepare(
    list(gene_stats_uri = gene_stats_uri),
    databases = list("KEGG", "GO"), methods = list("ora", "fgsea"),
    org = "hsa", namespace = "biological_process"
  )
  withr::defer(cleanup_prep(res))
  expect_null(res$error)
  expect_length(res$combos, 4L)
})

test_that("pa_build_response surfaces all-failed jobs as error", {
  prep <- list(databases = list("KEGG"), org = "hsa", run_id = "x",
               combos = list(list(job_name = "j1",
                                  csv_url = "u", csv_fname = "f.csv")),
               skipped_kegg_only = character(0L))
  jobs <- list(list(success = FALSE, job_name = "j1", stderr = "boom"))
  res <- pa_build_response(jobs, prep)
  expect_true(is_tool_error(res))
})

test_that("pa_build_response emits resource_links for successful jobs only", {
  prep <- list(databases = list("KEGG"), org = "hsa", run_id = "x",
               combos = list(
                 list(job_name = "j1", csv_url = "u1", csv_fname = "f1.csv"),
                 list(job_name = "j2", csv_url = "u2", csv_fname = "f2.csv")),
               skipped_kegg_only = character(0L))
  jobs <- list(list(success = TRUE, job_name = "j1", stderr = ""),
               list(success = FALSE, job_name = "j2", stderr = "boom"))
  res <- pa_build_response(jobs, prep)
  expect_false(isTRUE(res$isError))
  types <- vapply(res$content, function(c) c$type, character(1L))
  expect_setequal(types, c("text", "resource_link"))
  expect_equal(sum(types == "resource_link"), 1L)
})
