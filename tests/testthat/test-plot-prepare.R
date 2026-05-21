test_that(".PLOT_DISPATCH covers all canonical plot types", {
  expect_setequal(
    names(.PLOT_DISPATCH),
    c("volcano", "bar", "heatmap", "forest", "venn", "network", "kegg", "ma")
  )
})

test_that("plot_results_prepare rejects missing result_type", {
  res <- plot_results_prepare(list(result_type = "",
                                    types = list("volcano"),
                                    result_uri = "http://x"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "result_type")
})

test_that("plot_results_prepare rejects empty plot types", {
  res <- plot_results_prepare(list(result_type = "de_result",
                                    types = list(),
                                    result_uri = "http://x"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "type or types is required")
})

test_that("plot_results_prepare rejects unknown plot types", {
  res <- plot_results_prepare(list(result_type = "de_result",
                                    types = list("wat"),
                                    result_uri = "http://x"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Unknown plot type")
})

test_that("plot_results_prepare rejects incompatible plot type / result type", {
  res <- plot_results_prepare(list(result_type = "pa_result",
                                    types = list("kegg"),
                                    result_uri = "http://x"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "not compatible with result_type")
})

test_that("plot_results_prepare rejects kegg plot without kegg_pathway_id", {
  res <- plot_results_prepare(list(result_type = "de_result",
                                    types = list("kegg"),
                                    result_uri = "http://x"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "kegg_pathway_id")
})

test_that("plot_results_prepare rejects missing URIs", {
  res <- plot_results_prepare(list(result_type = "de_result",
                                    types = list("volcano")))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "result_uri")
})

test_that("plot_results_prepare reports fetch failures", {
  res <- plot_results_prepare(list(
    result_type = "de_result",
    types = list("volcano"),
    result_uri = "http://127.0.0.1:1/never.csv"))
  expect_false(is.null(res$error))
  expect_match(error_text(res$error), "Failed to fetch")
})

test_that("elicit_plot_args fills missing plot types from elicitation", {
  ctx <- build_mock_ctx(list(
    list(action = "accept", content = list(types = list("volcano")))
  ))
  args <- elicit_plot_args(list(result_type = "de_result",
                                 result_uri = "http://x"), ctx)
  expect_false(is_tool_error(args))
  expect_setequal(unlist(args$types), "volcano")
})

test_that("elicit_plot_args returns error when no result_type given", {
  ctx <- build_mock_ctx(list())
  res <- elicit_plot_args(list(), ctx)
  expect_true(is_tool_error(res))
})

test_that("elicit_plot_args returns error when no plot type and no elicitation cap", {
  ctx <- build_mock_ctx(list())
  ctx$client_capabilities <- list()
  res <- elicit_plot_args(list(result_type = "de_result"), ctx)
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "Valid for 'de_result'")
})

test_that("elicit_plot_args asks for top_n_genes on DE heatmap", {
  ctx <- build_mock_ctx(list(
    list(action = "accept", content = list(top_n_genes = 25L))
  ))
  args <- elicit_plot_args(
    list(result_type = "de_result",
         result_uri = "http://x",
         types = list("heatmap")),
    ctx)
  expect_false(is_tool_error(args))
  expect_equal(args$top_n_genes, 25L)
})

test_that("elicit_plot_args asks for top_n_pathways on PA bar/heatmap/forest/network", {
  ctx <- build_mock_ctx(list(
    list(action = "accept", content = list(top_n_pathways = 30L))
  ))
  args <- elicit_plot_args(
    list(result_type = "pa_result",
         result_uri = "http://x",
         types = list("bar")),
    ctx)
  expect_false(is_tool_error(args))
  expect_equal(args$top_n_pathways, 30L)
})

test_that("elicit_plot_args skips top_n elicitation when value provided", {
  ctx <- build_mock_ctx(list())
  args <- elicit_plot_args(
    list(result_type = "de_result",
         result_uri = "http://x",
         types = list("heatmap"),
         top_n_genes = 100L),
    ctx)
  expect_false(is_tool_error(args))
  expect_equal(args$top_n_genes, 100L)
  expect_length(ctx$.elicit_calls, 0L)
})
