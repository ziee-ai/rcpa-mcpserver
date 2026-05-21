test_that("build_rcpa_server registers all 6 tools", {
  srv <- build_rcpa_server()
  tool_names <- ls(srv$tools, all.names = TRUE)
  expect_setequal(
    tool_names,
    c("validate_input_file",
      "run_de_analysis",
      "run_pathway_analysis",
      "plot_results",
      "run_consensus_analysis",
      "run_meta_analysis")
  )
})

test_that("each registered tool has a valid input_schema", {
  srv <- build_rcpa_server()
  for (n in ls(srv$tools)) {
    t <- get(n, envir = srv$tools)
    expect_true(is.list(t$input_schema), info = n)
    expect_equal(t$input_schema$type, "object", info = n)
    expect_true(is.list(t$input_schema$properties) ||
                is.environment(t$input_schema$properties), info = n)
  }
})

test_that("elicitation tools are marked bidirectional", {
  srv <- build_rcpa_server()
  expect_true(get("run_pathway_analysis",
                  envir = srv$tools)$bidirectional)
  expect_true(get("plot_results", envir = srv$tools)$bidirectional)
  expect_true(get("run_consensus_analysis",
                  envir = srv$tools)$bidirectional)
  expect_true(get("run_meta_analysis", envir = srv$tools)$bidirectional)
})

test_that("non-elicitation tools run on the daemon pool", {
  srv <- build_rcpa_server()
  expect_false(get("validate_input_file",
                   envir = srv$tools)$bidirectional)
  expect_false(get("run_de_analysis",
                   envir = srv$tools)$bidirectional)
})

test_that("server advertises only the kinds we registered", {
  srv <- build_rcpa_server()
  caps <- srv$capabilities()
  expect_false(is.null(caps$tools))
  expect_null(caps$resources)
  expect_null(caps$prompts)
})

test_that("tool annotations declare openWorldHint for fetching tools", {
  srv <- build_rcpa_server()
  for (n in c("validate_input_file", "run_de_analysis",
              "run_pathway_analysis", "plot_results",
              "run_consensus_analysis", "run_meta_analysis")) {
    t <- get(n, envir = srv$tools)
    expect_true(isTRUE(t$annotations$openWorldHint), info = n)
  }
})
