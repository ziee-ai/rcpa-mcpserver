# Unit tests for result_uri() — the URI builder shared by all 6 tool handlers.
# The helper decides between an HTTP URL (default; what spawn_static_server
# serves) and a file:// path (stdio + RCPA_RESULTS_MODE=file). These tests
# guarantee the switch behaves correctly for both transports without spawning
# a subprocess.

# result_uri and .pkg_env are package internals — pull them through ::: once.
result_uri <- rcpa.mcpserver:::result_uri
.pkg_env   <- rcpa.mcpserver:::.pkg_env   # environment: reference semantics

test_that("result_uri defaults to http when no env var or arg is set", {
  withr::with_envvar(c(RCPA_RESULTS_MODE = NA), {
    uri <- result_uri("run_X", "out.csv")
    expect_match(uri, "^http://")
    expect_match(uri, "/results/run_X/out\\.csv$")
  })
})

test_that("result_uri('http') uses base_url()", {
  uri <- result_uri("run_X", "out.csv", mode = "http")
  expect_identical(uri, paste0(rcpa.mcpserver:::base_url(),
                               "/results/run_X/out.csv"))
})

test_that("result_uri('file') emits a file:// path under results_dir()", {
  rdir <- withr::local_tempdir()
  old <- .pkg_env$results_dir
  .pkg_env$results_dir <- rdir
  withr::defer(.pkg_env$results_dir <- old)

  uri <- result_uri("run_X", "out.csv", mode = "file")
  expect_match(uri, "^file://")
  expect_identical(uri,
                   paste0("file://",
                          normalizePath(file.path(rdir, "run_X", "out.csv"),
                                        mustWork = FALSE)))
})

test_that("RCPA_RESULTS_MODE=file env var flips the default to file://", {
  withr::with_envvar(c(RCPA_RESULTS_MODE = "file"), {
    uri <- result_uri("run_X", "out.csv")
    expect_match(uri, "^file://")
  })
})

test_that("RCPA_RESULTS_MODE=http env var keeps http URLs", {
  withr::with_envvar(c(RCPA_RESULTS_MODE = "http"), {
    uri <- result_uri("run_X", "out.csv")
    expect_match(uri, "^http://")
  })
})

test_that("result_uri('file') preserves a filename with a subpath", {
  rdir <- withr::local_tempdir()
  old <- .pkg_env$results_dir
  .pkg_env$results_dir <- rdir
  withr::defer(.pkg_env$results_dir <- old)

  uri <- result_uri("run_X", "plots/x.png", mode = "file")
  expect_match(uri, "plots/x\\.png$")
})

test_that("result_uri('file') builds a URI even when the file doesn't exist yet", {
  # The handler builds the URI before the subprocess writes the CSV/PNG, so
  # normalizePath(mustWork=FALSE) is load-bearing. Verify the helper does NOT
  # fail when the run dir / file are missing.
  rdir <- withr::local_tempdir()
  old <- .pkg_env$results_dir
  .pkg_env$results_dir <- rdir
  withr::defer(.pkg_env$results_dir <- old)

  uri <- expect_no_error(result_uri("run_does_not_exist_yet",
                                     "out.csv", mode = "file"))
  expect_match(uri, "^file://")
  expect_match(uri, "run_does_not_exist_yet/out\\.csv$")
})

test_that("result_uri('file') is absolute when results_dir() is absolute", {
  # The realistic input: .onLoad and start_stdio_server both source results_dir
  # from tempdir() or an env var that's typically absolute. Lock in the
  # invariant for that path.
  rdir <- withr::local_tempdir()
  # withr::local_tempdir returns an absolute path on every platform R supports.
  expect_true(startsWith(rdir, "/") || grepl("^[A-Za-z]:", rdir))
  old <- .pkg_env$results_dir
  .pkg_env$results_dir <- rdir
  withr::defer(.pkg_env$results_dir <- old)

  uri <- result_uri("run_X", "out.csv", mode = "file")
  path <- sub("^file://", "", uri)
  expect_true(startsWith(path, "/") || grepl("^[A-Za-z]:", path),
              info = paste("expected absolute path, got", path))
})

test_that("every tool that builds a result URI uses result_uri()", {
  # Regression guard: if a future tool adds a hand-rolled paste0(base_url(),
  # '/results/', ...) string, file:// mode will silently break for it. The
  # helper itself is the only place that pattern is allowed.
  pkg_root <- testthat::test_path("..", "..")
  r_files <- list.files(file.path(pkg_root, "R"),
                        pattern = "^tool_.*\\.R$", full.names = TRUE)
  expect_gt(length(r_files), 0L)
  for (f in r_files) {
    contents <- readLines(f, warn = FALSE)
    bad <- grep('paste0\\(base_url\\(\\), "/results/', contents, value = TRUE)
    expect_length(bad, 0L)
  }
  # And result_uri() shows up in at least the five tool files that emit links.
  hits <- vapply(r_files, function(f) {
    any(grepl("result_uri\\(", readLines(f, warn = FALSE)))
  }, logical(1L))
  expect_gte(sum(hits), 5L)
})
