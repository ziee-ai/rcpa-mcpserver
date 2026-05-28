# Full enforced-auth suite: rcpa.mcpserver with RCPA_AUTH=on.

skip_if_no_auth_deps()

srv <- NULL
withr::defer(teardown_rcpa(srv), testthat::teardown_env())

start_once <- function() {
  if (!is.null(srv) && srv$process$is_alive()) return(invisible(srv))
  srv <<- spawn_rcpa(mode = "on")
  invisible(srv)
}

init_session <- function(srv, token) {
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

# ---- 401 / 403 matrix ---------------------------------------------------

test_that("POST /mcp without bearer is 401 + WWW-Authenticate", {
  start_once()
  r <- http_call(srv$mcp_url, "POST",
                 headers = auth_headers(srv, token = NULL),
                 body = list(jsonrpc = "2.0", id = 1L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 401L)
  www <- httr2::resp_header(r, "WWW-Authenticate") %||% ""
  expect_match(www, "Bearer", fixed = TRUE)
})

test_that("malformed Authorization header is rejected", {
  start_once()
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = NULL),
                             Authorization = "Token nope"),
                 body = list(jsonrpc = "2.0", id = 2L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 401L)
})

test_that("wrong bearer is rejected", {
  start_once()
  r <- http_call(srv$mcp_url, "POST",
                 headers = auth_headers(srv, token = "definitely-not-the-token"),
                 body = list(jsonrpc = "2.0", id = 3L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 401L)
})

# ---- Admin surface ------------------------------------------------------

test_that("/admin/healthz with bootstrap is 200", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/healthz"), "GET",
                 headers = auth_headers(srv))
  expect_equal(httr2::resp_status(r), 200L)
  expect_equal(jsbody(r)$status, "ok")
})

test_that("admin REST: create user, list, delete", {
  start_once()
  base <- paste0(srv$url, "/admin/users")
  r1 <- http_call(base, "POST", headers = auth_headers(srv),
                  body = list(username = "alice"))
  expect_equal(httr2::resp_status(r1), 201L)
  uid <- jsbody(r1)$id
  expect_true(nzchar(uid))

  r2 <- http_call(base, "GET", headers = auth_headers(srv))
  expect_equal(httr2::resp_status(r2), 200L)
  unames <- vapply(jsbody(r2)$users, function(u) u$username,
                   character(1L))
  expect_true("alice" %in% unames)

  r3 <- http_call(paste0(base, "/", uid), "DELETE",
                  headers = auth_headers(srv))
  expect_equal(httr2::resp_status(r3), 204L)
})

# ---- Mint -> use -> revoke -> 401 ---------------------------------------

test_that("mint a user token; authorized /mcp call succeeds", {
  start_once()
  # set up bob
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST",
                  headers = auth_headers(srv),
                  body = list(username = "bob"))
  expect_equal(httr2::resp_status(cu), 201L)
  uid <- jsbody(cu)$id

  m <- http_call(paste0(srv$url, "/admin/tokens/mint"), "POST",
                 headers = auth_headers(srv),
                 body = list(user_id = uid, name = "ci",
                             scopes = list(),
                             ttl = 3600L))
  expect_equal(httr2::resp_status(m), 200L)
  mint <- jsbody(m)
  expect_true(nzchar(mint$jti))
  expect_match(mint$token, "^eyJ")

  # use the JWT against /mcp — initialize first to allocate a session
  sid <- init_session(srv, token = mint$token)
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = mint$token),
                             `Mcp-Session-Id` = sid),
                 body = list(jsonrpc = "2.0", id = 2L,
                             method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r), 200L)
  names <- vapply(jsbody(r)$result$tools, function(t) t$name,
                  character(1L))
  expect_true("validate_input_file" %in% names)
})

test_that("revocation: after POST /revoke the same JWT is 401", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST",
                  headers = auth_headers(srv),
                  body = list(username = paste0("carol-",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"), "POST",
                        headers = auth_headers(srv),
                        body = list(user_id = uid, name = "revoke-me",
                                    scopes = list(),
                                    ttl = 600L)))
  sid <- init_session(srv, token = m$token)
  ok <- http_call(srv$mcp_url, "POST",
                  headers = c(auth_headers(srv, token = m$token),
                              `Mcp-Session-Id` = sid),
                  body = list(jsonrpc = "2.0", id = 2L,
                              method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(ok), 200L)

  rev <- http_call(sprintf("%s/admin/tokens/%s/revoke", srv$url,
                            m$jti),
                   "POST", headers = auth_headers(srv))
  expect_equal(httr2::resp_status(rev), 204L)

  dead <- http_call(srv$mcp_url, "POST",
                    headers = c(auth_headers(srv, token = m$token),
                                `Mcp-Session-Id` = sid),
                    body = list(jsonrpc = "2.0", id = 3L,
                                method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(dead), 401L)
})

# ---- Authorized live tool call ------------------------------------------

test_that("tools/call validate_input_file works with the minted JWT", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST",
                  headers = auth_headers(srv),
                  body = list(username = paste0("dave-",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"), "POST",
                        headers = auth_headers(srv),
                        body = list(user_id = uid, name = "live",
                                    scopes = list(), ttl = 600L)))
  sid <- init_session(srv, token = m$token)
  csv_path <- file.path(srv$results_dir, "auth-on-smoke.csv")
  writeLines(c("\"\",\"s1\",\"s2\"",
               "\"g1\",1,2", "\"g2\",3,4", "\"g3\",5,6"),
             csv_path)
  r <- http_call(srv$mcp_url, "POST",
                 headers = c(auth_headers(srv, token = m$token),
                             `Mcp-Session-Id` = sid),
                 body = list(jsonrpc = "2.0", id = 2L,
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

# ---- Admin SPA shell ---------------------------------------------------

test_that("/admin/ui returns the SPA shell (HTML, no auth required)", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/ui"), "GET",
                 headers = c(Origin = "http://127.0.0.1"))
  expect_equal(httr2::resp_status(r), 200L)
  ct <- httr2::resp_header(r, "Content-Type") %||% ""
  expect_match(ct, "text/html")
  body <- httr2::resp_body_string(r)
  expect_true(grepl("/admin/ui/assets/", body, fixed = TRUE))
})

test_that("/admin/ui/<deep-link> falls back to index.html", {
  start_once()
  r <- http_call(paste0(srv$url, "/admin/ui/users"), "GET",
                 headers = c(Origin = "http://127.0.0.1"))
  expect_equal(httr2::resp_status(r), 200L)
  body <- httr2::resp_body_string(r)
  expect_true(grepl("<div id=\"root\">", body, fixed = TRUE))
})

# ---- Non-admin user gets 403 on /admin/* -------------------------------

test_that("non-admin user's JWT is rejected on /admin/* with 403", {
  start_once()
  cu <- http_call(paste0(srv$url, "/admin/users"), "POST",
                  headers = auth_headers(srv),
                  body = list(username = paste0("intern-",
                                                 as.integer(Sys.time()))))
  uid <- jsbody(cu)$id
  m <- jsbody(http_call(paste0(srv$url, "/admin/tokens/mint"), "POST",
                        headers = auth_headers(srv),
                        body = list(user_id = uid, name = "tk",
                                    scopes = list(), ttl = 600L)))
  r <- http_call(paste0(srv$url, "/admin/users"), "GET",
                 headers = auth_headers(srv, token = m$token))
  expect_equal(httr2::resp_status(r), 403L)
  # Same token still works on /mcp
  sid <- init_session(srv, token = m$token)
  r2 <- http_call(srv$mcp_url, "POST",
                  headers = c(auth_headers(srv, token = m$token),
                              `Mcp-Session-Id` = sid),
                  body = list(jsonrpc = "2.0", id = 2L,
                              method = "tools/list", params = list()))
  expect_equal(httr2::resp_status(r2), 200L)
})

# ---- Persistence across restart ----------------------------------------

test_that("users persist when the SQLite path is reused after restart", {
  skip_if_no_auth_deps()
  db_path <- tempfile(fileext = ".db")
  s1 <- spawn_rcpa(mode = "on", db_path = db_path)
  on.exit(teardown_rcpa(s1), add = TRUE)
  cu <- http_call(paste0(s1$url, "/admin/users"), "POST",
                  headers = auth_headers(s1),
                  body = list(username = "persistent"))
  expect_equal(httr2::resp_status(cu), 201L)

  # Shut down s1 cleanly so the SQLite WAL is checkpointed.
  s1$process$kill()
  Sys.sleep(0.5)

  s2 <- spawn_rcpa(mode = "on", db_path = db_path,
                   env_overrides = list(
                     MCPSERVER_ADMIN_TOKEN = s1$bootstrap_token))
  on.exit(teardown_rcpa(s2), add = TRUE)
  r <- http_call(paste0(s2$url, "/admin/users"), "GET",
                 headers = auth_headers(s2,
                                        token = s1$bootstrap_token))
  expect_equal(httr2::resp_status(r), 200L)
  unames <- vapply(jsbody(r)$users, function(u) u$username,
                   character(1L))
  expect_true("persistent" %in% unames)
})
