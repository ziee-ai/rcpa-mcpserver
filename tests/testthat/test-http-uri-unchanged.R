# Backward-compat guard for the result_uri() refactor (R/zzz.R).
#
# When the HTTP entrypoint runs, RCPA_RESULTS_MODE is unset, so result_uri()
# must keep emitting http(s) URLs — otherwise the static file server can't
# resolve them and every HTTP client breaks. This is locked in by the heavy
# test in test-http-integration.R, but that test is gated on RCPA + Bioconductor
# packages; this file is a tiny, always-runnable smoke test against the helper
# directly.

result_uri <- rcpa.mcpserver:::result_uri

test_that("result_uri() defaults to http when RCPA_RESULTS_MODE is unset", {
  withr::with_envvar(c(RCPA_RESULTS_MODE = NA), {
    uri <- result_uri("run_X", "out.csv")
    expect_match(uri, "^http://",
      info = "HTTP transport relies on result_uri() defaulting to http URLs")
  })
})

test_that("result_uri('http') always produces an http URL even with file env", {
  withr::with_envvar(c(RCPA_RESULTS_MODE = "file"), {
    uri <- result_uri("run_X", "out.csv", mode = "http")
    expect_match(uri, "^http://",
      info = "explicit mode='http' must override the env var")
  })
})
