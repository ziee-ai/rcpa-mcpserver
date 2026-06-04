# Protocol-level stdio integration + the light validate_input_file tool.
# Always runs (no scientific deps needed). Mirrors test-http-integration.R
# plus stdio-only cases (parse error, batch, notifications, logging, script
# launcher smoke test).

test_that("stdio initialize returns serverInfo and capabilities", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  init <- stdio_initialize(srv)
  expect_equal(init$result$serverInfo$name, "rcpa-mcpserver")
  expect_true("tools" %in% names(init$result$capabilities))
})

test_that("stdio tools/list returns all 6 RCPA tools", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  send_msg(srv, list(jsonrpc = "2.0", id = 2L, method = "tools/list"))
  resp <- read_msg(srv)
  names_seen <- vapply(resp$result$tools, function(t) t$name, character(1L))
  expect_setequal(names_seen, c(
    "validate_input_file",
    "run_de_analysis",
    "run_pathway_analysis",
    "plot_results",
    "run_consensus_analysis",
    "run_meta_analysis"))
})

test_that("stdio responds to ping with an empty result", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  send_msg(srv, list(jsonrpc = "2.0", id = 20L, method = "ping"))
  resp <- read_msg(srv)
  expect_equal(resp$id, 20)
  expect_equal(length(resp$result), 0L)
})

test_that("stdio validate_input_file validates an expression matrix end-to-end", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  csv_path <- write_expr_csv(n_genes = 12L, n_samples = 6L)
  withr::defer(unlink(csv_path))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 3L, "validate_input_file",
    list(file_uri  = file_uri(csv_path),
         file_type = "expression_matrix"),
    timeout_ms = 120000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  txt <- result_text(resp)
  expect_match(txt, '"valid":true')
  expect_match(txt, '"file_type":"expression_matrix"')
  expect_match(txt, '"n_genes":12')
  expect_match(txt, '"n_samples":6')
})

test_that("stdio validate_input_file validates an experimental design end-to-end", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  samples <- paste0("sample", 1:6)
  design_path <- write_design_csv(samples)
  withr::defer(unlink(design_path))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 4L, "validate_input_file",
    list(file_uri  = file_uri(design_path),
         file_type = "experimental_design"),
    timeout_ms = 120000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  txt <- result_text(resp)
  expect_match(txt, '"valid":true')
  expect_match(txt, '"file_type":"experimental_design"')
})

test_that("stdio validate_input_file surfaces a missing file_uri as isError", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 5L, "validate_input_file",
    list(file_uri = "", file_type = "expression_matrix"))
  expect_true(isTRUE(resp$result$isError))
  expect_match(result_text(resp), "file_uri is required")
})

test_that("stdio request for an unknown tool yields a JSON-RPC error", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 6L, "does_not_exist", list())
  # Dispatch is transport-agnostic, so stdio must report unknown tools the
  # same way the HTTP transport does (test-http-integration.R): a JSON-RPC
  # error envelope (not an isError tool result) with an "unknown tool" message.
  expect_false(is.null(resp$error))
  expect_match(resp$error$message, "unknown tool")
})

test_that("stdio rejects malformed JSON with -32700 parse_error", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  send_raw(srv, "{this is not valid json")
  resp <- read_msg(srv)
  expect_equal(resp$error$code, -32700L)
})

test_that("stdio handles a JSON-RPC batch (array of requests)", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  send_raw(srv, paste0(
    '[{"jsonrpc":"2.0","id":10,"method":"ping"},',
    '{"jsonrpc":"2.0","id":11,"method":"tools/list"}]'))
  r1 <- read_msg(srv)
  r2 <- read_msg(srv)
  expect_setequal(c(r1$id, r2$id), c(10, 11))
})

test_that("stdio does not send a reply for notifications (no id)", {
  skip_if_no_stdio_deps()
  srv <- spawn_rcpa_stdio()
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  # Send a no-op notification (no id). The server must NOT reply. Then send
  # a real request with an id and confirm that's the very next thing we read
  # — proving no spurious notification reply was emitted.
  send_msg(srv, list(jsonrpc = "2.0", method = "notifications/cancelled",
                     params = list(requestId = 9999L)))
  send_msg(srv, list(jsonrpc = "2.0", id = 77L, method = "ping"))
  resp <- read_msg(srv)
  expect_equal(resp$id, 77)
})

test_that("RCPA_LOG keeps stdout clean and serve_io activates the file sink", {
  skip_if_no_stdio_deps()
  log_file <- tempfile(fileext = ".log")
  withr::defer(unlink(log_file))

  # Pass RCPA_LOG via the child env. The helper uses Sys.getenv() to fold
  # process-level env into the child, so set it before spawning.
  withr::with_envvar(c(RCPA_LOG = log_file), {
    srv <- spawn_rcpa_stdio()
    withr::defer(stop_rcpa_stdio(srv))

    init <- stdio_initialize(srv)
    # If [Info] lines were leaking onto stdout, read_msg would have grabbed
    # them instead of the JSON-RPC envelope and parsing would fail — so a
    # successful initialize implicitly proves stdout is clean.
    expect_equal(init$result$serverInfo$name, "rcpa-mcpserver")
  })

  # serve_io opens the log file via file(path, open="a") which creates it
  # on disk — so the file existing is proof the sink path executed.
  Sys.sleep(0.5)
  expect_true(file.exists(log_file),
              info = "RCPA_LOG: serve_io should have created the sink file")
})

test_that("inst/run-stdio.R script launches and replies to initialize", {
  skip_if_no_stdio_deps()
  runner <- system.file("run-stdio.R", package = "rcpa.mcpserver")
  skip_if(!nzchar(runner) || !file.exists(runner),
          "run-stdio.R not installed")

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  child_env["RCPA_RESULTS_MODE"] <- "file"

  p <- processx::process$new("Rscript", c(runner),
                             stdin = "|", stdout = "|", stderr = "|",
                             env = child_env)
  withr::defer(tryCatch(p$kill(), error = function(e) NULL))
  Sys.sleep(3)

  init_line <- paste0(
    jsonlite::toJSON(list(jsonrpc = "2.0", id = 1L, method = "initialize",
                          params = list(protocolVersion = "2025-06-18",
                                        capabilities = list())),
                     auto_unbox = TRUE),
    "\n")
  p$write_input(init_line)

  # Read one line of stdout with a timeout, mirroring helper-stdio's reader.
  buf <- ""
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < 20) {
    p$poll_io(200)
    chunk <- p$read_output()
    if (nchar(chunk) > 0L) {
      buf <- paste0(buf, chunk)
      if (grepl("\n", buf, fixed = TRUE)) break
    }
    if (!p$is_alive()) break
  }
  first_line <- strsplit(buf, "\n", fixed = TRUE)[[1L]][1L]
  expect_true(nzchar(first_line %||% ""),
              info = paste("stderr:",
                           paste(p$read_error_lines(), collapse = " | ")))
  init <- jsonlite::fromJSON(first_line, simplifyVector = FALSE)
  expect_equal(init$result$serverInfo$name, "rcpa-mcpserver")
})
