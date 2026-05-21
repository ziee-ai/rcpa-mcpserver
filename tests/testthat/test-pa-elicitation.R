test_that("elicit_pa_args fills missing databases/methods via request_elicitation", {
  ctx <- build_mock_ctx(list(
    list(action = "accept",
         content = list(databases = list("KEGG"),
                        methods   = list("ora")))
  ))
  args <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         org = "hsa", namespace = "biological_process"),
    ctx)
  expect_false(is_tool_error(args))
  expect_setequal(unlist(args$databases), "KEGG")
  expect_setequal(unlist(args$methods), "ora")
  expect_length(ctx$.elicit_calls, 1L)
})

test_that("elicit_pa_args prompts for namespace when GO is selected and no namespace given", {
  ctx <- build_mock_ctx(list(
    list(action = "accept",
         content = list(namespace = "molecular_function"))
  ))
  args <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         databases = list("GO"), methods = list("ora"),
         org = "hsa"),
    ctx)
  expect_false(is_tool_error(args))
  expect_equal(args$namespace, "molecular_function")
})

test_that("elicit_pa_args prompts for org when missing", {
  ctx <- build_mock_ctx(list(
    list(action = "accept", content = list(org = "mmu"))
  ))
  args <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         databases = list("KEGG"), methods = list("ora"),
         namespace = "biological_process"),
    ctx)
  expect_false(is_tool_error(args))
  expect_equal(args$org, "mmu")
})

test_that("elicit_pa_args makes no calls when everything is provided", {
  ctx <- build_mock_ctx(list())  # empty queue means any call would error
  args <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         databases = list("KEGG"), methods = list("ora"),
         org = "hsa", namespace = "biological_process"),
    ctx)
  expect_false(is_tool_error(args))
  expect_length(ctx$.elicit_calls, 0L)
})

test_that("elicit_pa_args returns tool error when client declined", {
  ctx <- build_mock_ctx(list(
    list(action = "decline", content = list())
  ))
  res <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         org = "hsa", namespace = "biological_process"),
    ctx)
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "declined")
})

test_that("elicit_pa_args returns tool error when client doesn't support elicitation", {
  ctx <- build_mock_ctx(list())
  ctx$client_capabilities <- list()  # no elicitation cap
  res <- elicit_pa_args(
    list(gene_stats_uri = "http://example.com/gs.csv",
         org = "hsa", namespace = "biological_process"),
    ctx)
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "databases and methods are required")
})

test_that("elicit_pa_args picks gene_list schema when gene_list input given", {
  ctx <- build_mock_ctx(list(
    list(action = "accept",
         content = list(databases = list("KEGG"),
                        methods = list("ora")))
  ))
  elicit_pa_args(
    list(gene_list = list("1", "2", "3"),
         org = "hsa", namespace = "biological_process"),
    ctx)
  schema <- ctx$.elicit_calls[[1L]]$schema
  enum <- schema$properties$methods$items$enum
  expect_setequal(as.character(enum), c("ora", "cepaORA"))
})

test_that("elicit_pa_args picks expression schema when expression input given", {
  ctx <- build_mock_ctx(list(
    list(action = "accept",
         content = list(databases = list("KEGG"),
                        methods = list("gsa")))
  ))
  elicit_pa_args(
    list(expression_uri = "http://example.com/expr.csv",
         experiment_design_uri = "http://example.com/design.csv",
         org = "hsa", namespace = "biological_process"),
    ctx)
  schema <- ctx$.elicit_calls[[1L]]$schema
  enum <- schema$properties$methods$items$enum
  expect_setequal(as.character(enum), "gsa")
})
