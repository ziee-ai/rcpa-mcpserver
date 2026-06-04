# Full HTTP integration: spawn serve_http in a child Rscript process,
# drive it via httr2. This is the pattern used by mcpserver-r's own
# tests because `serve_http(block=FALSE)` runs the request dispatcher
# on the same R thread - and `httr2::req_perform` in the same process
# would block that thread, deadlocking the server.

skip_if_no_http_deps <- function() {
  testthat::skip_if_not_installed("processx")
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("nanonext")
}

spawn_rcpa_server <- function(port, allow_local = TRUE,
                              startup_wait = 4) {
  runner_script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "suppressPackageStartupMessages(library(rcpa.mcpserver))",
    "srv <- build_rcpa_server()",
    sprintf("serve_http(srv, host = '127.0.0.1', port = %dL,", port),
    "           path = '/mcp',",
    "           allowed_origins = c('http://127.0.0.1', 'http://localhost'),",
    "           require_origin = FALSE,",
    "           stateless = TRUE,",
    "           daemons = 2L)"
  ), runner_script)
  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                                collapse = .Platform$path.sep)
  child_env["RCPA_ALLOW_LOCAL_URIS"] <- if (isTRUE(allow_local)) "TRUE" else "FALSE"
  p <- processx::process$new("Rscript", c(runner_script),
                              stdout = "|", stderr = "|",
                              env = child_env)
  Sys.sleep(startup_wait)
  if (!p$is_alive()) {
    err <- tryCatch(p$read_all_error(), error = function(e) "")
    out <- tryCatch(p$read_all_output(), error = function(e) "")
    stop(sprintf("server failed to start: stderr=%s\nstdout=%s", err, out))
  }
  list(process = p, port = port,
       url = sprintf("http://127.0.0.1:%d/mcp", port),
       runner_script = runner_script)
}

stop_rcpa_server <- function(server) {
  tryCatch(server$process$kill(), error = function(e) NULL)
  tryCatch(unlink(server$runner_script), error = function(e) NULL)
}

post <- function(server, body, timeout = 10) {
  req <- httr2::request(server$url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Origin = "http://127.0.0.1",
      `Content-Type` = "application/json",
      Accept = "application/json, text/event-stream"
    ) |>
    httr2::req_body_raw(charToRaw(body)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(timeout)
  httr2::req_perform(req)
}

pick_free_port <- function() {
  for (attempt in seq_len(20L)) {
    port <- sample(20000:60000, 1L)
    can_bind <- tryCatch({
      s <- nanonext::socket("rep")
      on.exit(nanonext::reap(s), add = TRUE)
      nanonext::listen(s, sprintf("tcp://127.0.0.1:%d", port))
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(can_bind)) return(port)
  }
  stop("could not find a free port after 20 attempts")
}

parse_body <- function(resp) {
  text <- httr2::resp_body_string(resp)
  # SSE-wrapped responses arrive when the server sends event-stream
  # frames. Strip the data: prefix for parsing.
  text <- gsub("\r\n", "\n", text)
  if (grepl("^event: |^data: ", text)) {
    m <- regmatches(text, regexpr("(?m)^data:\\s*(.+)$", text, perl = TRUE))
    if (length(m) > 0L) {
      json <- sub("^data:\\s*", "", m[[1L]])
      return(jsonlite::fromJSON(json, simplifyVector = FALSE))
    }
  }
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

test_that("HTTP server responds to initialize", {
  skip_if_no_http_deps()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port)
  withr::defer(stop_rcpa_server(srv))

  resp <- post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  expect_equal(body$jsonrpc, "2.0")
  expect_equal(body$id, 1L)
  expect_equal(body$result$serverInfo$name, "rcpa-mcpserver")
  expect_true("tools" %in% names(body$result$capabilities))
})

test_that("HTTP tools/list returns all 6 RCPA tools", {
  skip_if_no_http_deps()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port)
  withr::defer(stop_rcpa_server(srv))

  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  resp <- post(srv, '{"jsonrpc":"2.0","id":2,"method":"tools/list"}')
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  names_seen <- vapply(body$result$tools, function(t) t$name, character(1L))
  expect_setequal(names_seen, c(
    "validate_input_file", "run_de_analysis", "run_pathway_analysis",
    "plot_results", "run_consensus_analysis", "run_meta_analysis"))
})

test_that("HTTP tools/call invokes validate_input_file end-to-end", {
  skip_if_no_http_deps()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port)
  withr::defer(stop_rcpa_server(srv))

  expr <- tempfile(fileext = ".csv")
  writeLines(c("\"\",\"s1\",\"s2\",\"s3\"",
               "\"g1\",1.0,2.0,3.0",
               "\"g2\",4.0,5.0,6.0",
               "\"g3\",7.0,8.0,9.0"), expr)
  withr::defer(unlink(expr))

  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  body <- sprintf(
    '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"validate_input_file","arguments":{"file_uri":"file://%s","file_type":"expression_matrix"}}}',
    normalizePath(expr, winslash = "/"))
  resp <- post(srv, body, timeout = 120)
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  expect_true(is.null(body$error),
              info = if (!is.null(body$error)) body$error$message else "")
  expect_false(isTRUE(body$result$isError))
  txt <- body$result$content[[1L]]$text
  expect_match(txt, '"valid":true')
  expect_match(txt, '"n_genes":3')
})

test_that("HTTP tools/call surfaces tool errors via isError=TRUE", {
  skip_if_no_http_deps()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port)
  withr::defer(stop_rcpa_server(srv))

  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  resp <- post(srv,
    '{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"validate_input_file","arguments":{"file_uri":"","file_type":"expression_matrix"}}}',
    timeout = 15)
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  expect_true(isTRUE(body$result$isError))
  expect_match(body$result$content[[1L]]$text, "file_uri is required")
})

test_that("HTTP request for unknown tool yields JSON-RPC error", {
  skip_if_no_http_deps()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port)
  withr::defer(stop_rcpa_server(srv))

  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  resp <- post(srv,
    '{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"does_not_exist","arguments":{}}}')
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  expect_false(is.null(body$error))
  expect_match(body$error$message, "unknown tool")
})

test_that("HTTP run_de_analysis end-to-end with real RCPA + limma", {
  skip_if_no_http_deps()
  skip_if_no_rcpa()
  port <- pick_free_port()
  srv <- spawn_rcpa_server(port, startup_wait = 5)
  withr::defer(stop_rcpa_server(srv))

  expr_path <- fixture_path("small_expr.csv")
  design_path <- fixture_path("small_design.csv")
  post(srv,
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","clientInfo":{"name":"i","version":"0"},"capabilities":{}}}')
  body <- sprintf(
    '{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"run_de_analysis","arguments":{"expression_uri":"file://%s","experiment_design_uri":"file://%s","method":"limma","contrast":"Treatment - Control"}}}',
    normalizePath(expr_path, winslash = "/"), normalizePath(design_path, winslash = "/"))
  resp <- post(srv, body, timeout = 90)
  expect_equal(httr2::resp_status(resp), 200L)
  body <- parse_body(resp)
  expect_false(isTRUE(body$result$isError),
               info = if (isTRUE(body$result$isError))
                 body$result$content[[1L]]$text else "")
  # content[[1]]: metadata text; content[[2]]: resource_link to CSV
  expect_match(body$result$content[[1L]]$text,
               '"result_type":"de_result"')
  link <- body$result$content[[2L]]
  expect_equal(link$type, "resource_link")
  expect_match(link$uri, "^http")
  expect_equal(link$mimeType, "text/csv")
})
