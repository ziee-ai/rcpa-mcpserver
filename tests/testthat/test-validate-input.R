test_that("validate_input_file rejects missing file_uri", {
  res <- validate_input_file_handler(
    list(file_uri = "", file_type = "expression_matrix"),
    build_mock_ctx())
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "file_uri is required")
})

test_that("validate_input_file rejects unknown file_type", {
  res <- validate_input_file_handler(
    list(file_uri = "http://example.com/x.csv", file_type = "wat"),
    build_mock_ctx())
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "file_type must be")
})

test_that("validate_input_file reports fetch failures", {
  res <- validate_input_file_handler(
    list(file_uri = "http://127.0.0.1:1/never_resolves.csv",
         file_type = "expression_matrix"),
    build_mock_ctx())
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "Failed to fetch")
})

test_that("validate_input_file accepts a valid expression matrix via file://", {
  tmp <- write_expr_csv(n_genes = 6L, n_samples = 4L)
  withr::defer(unlink(tmp))
  # The handler refuses file:// URIs via is_safe_uri; we bypass that by
  # exercising the response builder directly. Routing is covered by tests
  # against test-uri-fetch with a mocked httr2.
  res <- validate_expr_response("expression_matrix",
                                validate_expression_matrix(tmp))
  expect_false(isTRUE(res$isError))
  expect_match(res$text, "\"valid\":true")
  expect_match(res$text, "\"n_genes\":6")
})

test_that("validate_expr_response surfaces issues on invalid input", {
  res <- validate_expr_response("expression_matrix",
                                list(valid = FALSE,
                                     issues = "bad",
                                     n_genes = 0L,
                                     n_samples = 0L,
                                     sample_names = character(0L),
                                     preview = ""))
  expect_true(is_tool_error(res))
  expect_match(error_text(res), "bad")
  expect_match(error_text(res), "expected_format")
})

test_that("validate_gene_stats_response carries n_genes through", {
  res <- validate_gene_stats_response("gene_stats",
                                       list(valid = TRUE, n_genes = 42L))
  expect_match(res$text, "\"n_genes\":42")
})

test_that("validate_design_response carries groups and pair flag", {
  res <- validate_design_response("experimental_design",
                                  list(valid = TRUE,
                                       n_samples = 4L,
                                       sample_names = c("a", "b", "c", "d"),
                                       groups = c("A", "B"),
                                       is_paired = TRUE))
  expect_match(res$text, "\"is_paired\":true")
  expect_match(res$text, "\"A\",\"B\"")
})

test_that("tool_validate_input_file returns a tool descriptor", {
  t <- tool_validate_input_file()
  expect_equal(attr(t, "mcp_kind"), "tool")
  expect_equal(t$name, "validate_input_file")
  expect_true(is.function(t$handler))
  expect_true(!is.null(t$input_schema))
  expect_true("required" %in% names(t$input_schema))
})
