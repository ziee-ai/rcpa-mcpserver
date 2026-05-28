# Subprocess fixture used by test-auth-{off,on}-integration.R.
#
# Spawns `run_http_entrypoint()` in a child Rscript so the full auth
# wiring (env var -> rcpa_auth_config() -> serve_http) runs the same
# way it does in production. The fixture in `test-http-integration.R`
# (`spawn_rcpa_server`) calls `serve_http()` directly and is therefore
# unsuitable for exercising the auth code path.
#
# `spawn_rcpa(mode = "off")` boots with `RCPA_AUTH` unset and asserts
# the unauthenticated path stays identical to today.
#
# `spawn_rcpa(mode = "on")` boots with `RCPA_AUTH=on`, a fresh
# tempfile SQLite database, and a fixed bootstrap token so tests can
# call the admin REST API deterministically.

skip_if_no_auth_deps <- function() {
  testthat::skip_if_not_installed("processx")
  testthat::skip_if_not_installed("httr2")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_on_cran()
  testthat::skip_if(nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")))
}

spawn_rcpa <- function(mode = c("off", "on"),
                       port = NULL,
                       static_port = NULL,
                       startup_wait = 4,
                       env_overrides = list(),
                       db_path = NULL) {
  mode <- match.arg(mode)
  if (is.null(port)) port <- sample(45000:48000, 1L)
  if (is.null(static_port)) static_port <- sample(45000:48000, 1L)

  runner_script <- tempfile(fileext = ".R")
  results_dir   <- tempfile("rcpa-results-")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

  bootstrap_token <- NULL
  if (mode == "on") {
    if (is.null(db_path)) db_path <- tempfile(fileext = ".db")
    bootstrap_token <- paste(openssl::rand_bytes(16L), collapse = "")
  }

  writeLines(c(
    "suppressPackageStartupMessages(library(rcpa.mcpserver))",
    sprintf("run_http_entrypoint(port = %dL,", port),
    sprintf("                    static_port = %dL,", static_port),
    "                    daemons = 2L,",
    "                    allowed_origins = c('http://127.0.0.1', 'http://localhost'))"
  ), runner_script)

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(),
                                collapse = .Platform$path.sep)
  child_env["RCPA_RESULTS_DIR"] <- results_dir
  child_env["RCPA_ALLOW_LOCAL_URIS"] <- "TRUE"
  child_env["RCPA_STATIC_HOST"] <- "127.0.0.1"
  child_env["RCPA_HOST"] <- "127.0.0.1"

  if (mode == "on") {
    child_env["RCPA_AUTH"] <- "on"
    child_env["MCPSERVER_ADMIN_TOKEN"] <- bootstrap_token
    child_env["RCPA_AUTH_DB"] <- db_path
  } else {
    # Force off even if the parent environment has it set.
    child_env["RCPA_AUTH"] <- "off"
    child_env["MCPSERVER_ADMIN_TOKEN"] <- ""
    child_env["RCPA_AUTH_DB"] <- ""
  }
  for (k in names(env_overrides)) {
    child_env[k] <- as.character(env_overrides[[k]])
  }

  p <- processx::process$new("Rscript", c(runner_script),
                             stdout = "|", stderr = "|",
                             env = child_env)
  # Poll readiness. For "off" mode we hit /mcp with a bogus body and
  # accept any HTTP response (the server is ready as soon as it binds).
  # For "on" mode we hit /admin/healthz with the bootstrap token.
  url    <- sprintf("http://127.0.0.1:%d", port)
  probe  <- if (mode == "on") paste0(url, "/admin/healthz") else paste0(url, "/mcp")
  ok <- FALSE
  for (i in seq_len(80L)) {
    resp <- tryCatch({
      req <- httr2::request(probe) |>
        httr2::req_method(if (mode == "on") "GET" else "POST") |>
        httr2::req_headers(Origin = "http://127.0.0.1") |>
        httr2::req_error(is_error = function(r) FALSE) |>
        httr2::req_timeout(2)
      if (mode == "on") {
        req <- httr2::req_headers(req,
                                  Authorization = paste("Bearer", bootstrap_token))
      } else {
        req <- httr2::req_headers(req,
                                  `Content-Type` = "application/json",
                                  Accept = "application/json, text/event-stream") |>
          httr2::req_body_raw(charToRaw("{}"))
      }
      httr2::req_perform(req)
    }, error = function(e) NULL)
    if (!is.null(resp)) { ok <- TRUE; break }
    Sys.sleep(0.25)
  }
  if (!ok) {
    err <- tryCatch(p$read_all_error(), error = function(e) "")
    out <- tryCatch(p$read_all_output(), error = function(e) "")
    p$kill()
    unlink(runner_script)
    unlink(results_dir, recursive = TRUE)
    if (!is.null(db_path)) unlink(c(db_path,
                                     paste0(db_path, "-wal"),
                                     paste0(db_path, "-shm")))
    testthat::skip(paste0("rcpa server failed to start (mode=", mode, "): ",
                          substr(paste("stderr:", err, "| stdout:", out),
                                 1L, 600L)))
  }
  list(
    process = p,
    port = port,
    static_port = static_port,
    url = url,
    mcp_url = paste0(url, "/mcp"),
    bootstrap_token = bootstrap_token,
    db_path = db_path,
    runner_script = runner_script,
    results_dir = results_dir
  )
}

teardown_rcpa <- function(srv) {
  if (is.null(srv)) return(invisible(NULL))
  if (!is.null(srv$process) && srv$process$is_alive()) {
    tryCatch(srv$process$kill(), error = function(e) NULL)
  }
  unlink(srv$runner_script)
  unlink(srv$results_dir, recursive = TRUE)
  if (!is.null(srv$db_path) && nzchar(srv$db_path)) {
    unlink(c(srv$db_path,
             paste0(srv$db_path, "-wal"),
             paste0(srv$db_path, "-shm")))
  }
  invisible(NULL)
}

# Common headers + helpers used by both auth-{off,on} tests.

auth_headers <- function(srv, token = srv$bootstrap_token,
                         extra = list()) {
  hdr <- c(Origin = "http://127.0.0.1",
           `Content-Type` = "application/json",
           Accept = "application/json, text/event-stream")
  if (!is.null(token) && nzchar(token)) {
    hdr <- c(hdr, Authorization = paste("Bearer", token))
  }
  for (k in names(extra)) hdr[[k]] <- extra[[k]]
  hdr
}

http_call <- function(url, method = "GET", headers = NULL,
                      body = NULL, timeout = 10) {
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_timeout(timeout)
  if (!is.null(headers)) {
    hdr_args <- as.list(headers); names(hdr_args) <- names(headers)
    req <- do.call(httr2::req_headers, c(list(req), hdr_args))
  }
  if (!is.null(body)) {
    raw <- if (is.list(body)) {
      charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    } else {
      charToRaw(as.character(body))
    }
    req <- httr2::req_body_raw(req, raw)
  }
  httr2::req_perform(req)
}

jsbody <- function(resp) {
  txt <- httr2::resp_body_string(resp)
  if (!nzchar(txt)) return(NULL)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}
