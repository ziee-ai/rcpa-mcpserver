test_that("mcp_tool_error builds an isError response with text content", {
  err <- mcp_tool_error("something failed")
  expect_true(is.list(err))
  expect_true(isTRUE(err$isError))
  expect_true(is_tool_error(err))
  expect_match(err$content[[1L]]$text, "something failed")
})

test_that("mcp_tool_error appends key:value details", {
  err <- mcp_tool_error("bad input", hint = "do X", expected_format = "CSV")
  txt <- err$content[[1L]]$text
  expect_match(txt, "bad input")
  expect_match(txt, "hint: do X")
  expect_match(txt, "expected_format: CSV")
})

test_that("mcp_tool_error joins multi-line issue vectors", {
  err <- mcp_tool_error(c("issue 1", "issue 2"))
  txt <- err$content[[1L]]$text
  expect_match(txt, "issue 1")
  expect_match(txt, "issue 2")
})

test_that("is_tool_error returns FALSE for non-error lists", {
  expect_false(is_tool_error(list(content = list())))
  expect_false(is_tool_error("not a list"))
  expect_false(is_tool_error(NULL))
})
