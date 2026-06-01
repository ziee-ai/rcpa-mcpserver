# Parity-with-today suite: rcpa.mcpserver with RCPA_AUTH unset / off.
# Asserts the unauthenticated path stays identical to the current
# (pre-auth) behavior and that the admin surface stays unmounted.

skip_if_no_auth_deps()

srv <- NULL
withr::defer(teardown_rcpa(srv), testthat::teardown_env())

start_once <- function() {
  if (!is.null(srv) && srv$process$is_alive()) return(invisible(srv))
  srv <<- spawn_rcpa(mode = "off")
  invisible(srv)
}

test_that("POST /mcp initialize works without any Authorization header", {
  start_once()
  r <- http_call(srv$mcp_url, "POST",
                 headers = auth_headers(srv, token = NULL),
                 body = list(jsonrpc = "2.0", id = 1L,
                             method = "initialize",
                             params = list(
                               protocolVersion = "2025-06-18",
                               capabilities = list())))
  expect_equal(httr2::resp_status(r), 200L)
  sid <- httr2::resp_header(r, "Mcp-Session-Id")
  expect_true(!is.null(sid) && nzchar(sid))
})

init_session <- function(srv, token = NULL) {
  r <- http_call(srv$mcp_url, "POST",
                 headers = auth_headers(srv, token = token),
                 body = list(jsonrpc = "2.0", id = 1L,
                             method = "initialize",
                             params = list(
                               protocolVersion = "2025-06-18",
                               capabilities = list())))
  expect_equal(httr2::resp_status(r), 200L)
  httr2::resp_header(r, "Mcp-Session-Id")
}

test_that("tools/list returns the six RCPA tools (no bearer)", {
  start_once()
  sid <- init_session(srv)
  expect_true(nzchar(sid %||% ""))
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = NULL),
                             `Mcp-Session-Id` = sid),
                 body = list(jsonrpc = "2.0", id = 2L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 200L)
  body <- jsbody(r)
  names <- vapply(body$result$tools, function(t) t$name,
                  character(1L))
  expect_setequal(names,
    c("validate_input_file", "run_de_analysis",
      "run_pathway_analysis", "plot_results",
      "run_consensus_analysis", "run_meta_analysis"))
})

test_that("a bogus bearer is still accepted when auth is off", {
  start_once()
  sid <- init_session(srv, token = "garbage")
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = "garbage"),
                             `Mcp-Session-Id` = sid),
                 body = list(jsonrpc = "2.0", id = 2L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 200L)
})

test_that("tools/call validate_input_file works without bearer (pipeline smoke)", {
  start_once()
  sid <- init_session(srv)
  csv_path <- file.path(srv$results_dir, "auth-off-smoke.csv")
  writeLines(c("\"\",\"s1\",\"s2\"",
               "\"g1\",1,2", "\"g2\",3,4", "\"g3\",5,6"),
             csv_path)
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = NULL),
                             `Mcp-Session-Id` = sid),
                 body = list(jsonrpc = "2.0", id = 3L,
                             method = "tools/call",
                             params = list(
                               name = "validate_input_file",
                               arguments = list(
                                 file_uri = paste0("file://", csv_path),
                                 file_type = "expression_matrix"))),
                 timeout = 30)
  expect_equal(httr2::resp_status(r), 200L)
  body <- jsbody(r)
  expect_false(isTRUE(body$result$isError))
  txt <- body$result$content[[1L]]$text
  parsed <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  expect_equal(parsed$n_genes, 3L)
})

test_that("admin REST surface is NOT mounted when auth is off", {
  start_once()
  r1 <- http_call(paste0(srv$url, "/admin/healthz"), "GET",
                  headers = c(Origin = "http://127.0.0.1"))
  expect_true(httr2::resp_status(r1) %in% c(404L, 405L))

  r2 <- http_call(paste0(srv$url, "/admin/users"), "GET",
                  headers = c(Origin = "http://127.0.0.1"))
  expect_true(httr2::resp_status(r2) %in% c(404L, 405L))
})

test_that("admin SPA shell is NOT mounted when auth is off", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/ui"), "GET",
                 headers = c(Origin = "http://127.0.0.1"))
  expect_true(httr2::resp_status(r) %in% c(404L, 405L))
})
