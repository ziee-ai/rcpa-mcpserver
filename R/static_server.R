#' Spawn a read-only static file HTTP server for the results directory
#'
#' Used by the run entry point to serve generated CSVs, RDS, and PNGs
#' back to clients via the \code{resource_link} URLs the analysis tools
#' emit. Defense-in-depth: rejects paths containing \code{..} to prevent
#' traversal, and refuses to serve anything outside the configured
#' results directory.
#'
#' @param dir Directory to serve (defaults to the package's results dir).
#' @param port TCP port (defaults to env var RCPA_STATIC_PORT or 9005).
#' @param host Bind host (defaults to env var RCPA_STATIC_HOST or "127.0.0.1").
#' @return The nanonext server handle. Caller is responsible for keeping
#'   it alive (or storing it in package state). Calling \code{$close()}
#'   on the handle shuts it down.
#' @export
spawn_static_server <- function(dir = results_dir(),
                                port = NULL,
                                host = NULL) {
  if (is.null(port)) {
    port <- as.integer(Sys.getenv("RCPA_STATIC_PORT", unset = "9005"))
  }
  if (is.null(host)) {
    host <- Sys.getenv("RCPA_STATIC_HOST", unset = "127.0.0.1")
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(dir, mustWork = TRUE)

  serve <- function(req) {
    raw_path <- req$URI %||% req$uri %||% req$path %||% "/"
    # Strip query string for path matching.
    raw_path <- sub("\\?.*$", "", raw_path)
    # Drop the /results prefix (the framework's path).
    rel <- sub("^/results/?", "", raw_path)
    rel <- sub("^/+", "", rel)

    if (grepl("(^|/)\\.\\.(/|$)", rel)) {
      return(list(status = 400L,
                  headers = list("Content-Type" = "text/plain",
                                 "Access-Control-Allow-Origin" = "*"),
                  body = "bad path"))
    }
    full <- file.path(root, rel)
    full_norm <- tryCatch(normalizePath(full, mustWork = FALSE),
                          error = function(e) "")
    if (!startsWith(full_norm, root)) {
      return(list(status = 403L,
                  headers = list("Content-Type" = "text/plain",
                                 "Access-Control-Allow-Origin" = "*"),
                  body = "forbidden"))
    }
    if (!file.exists(full) || dir.exists(full)) {
      return(list(status = 404L,
                  headers = list("Content-Type" = "text/plain",
                                 "Access-Control-Allow-Origin" = "*"),
                  body = "not found"))
    }
    body <- readBin(full, what = "raw",
                    n = file.info(full)$size %||% 0L)
    list(status = 200L,
         headers = list(
           "Content-Type" = guess_mime(full),
           "Access-Control-Allow-Origin" = "*",
           "Cache-Control" = "no-store"
         ),
         body = body)
  }

  url <- sprintf("http://%s:%d", host, port)
  srv <- nanonext::http_server(
    url,
    handlers = list(
      nanonext::handler("/results", serve, method = "GET", prefix = TRUE),
      nanonext::handler("/results", serve, method = "HEAD", prefix = TRUE)
    )
  )
  srv$start()
  srv
}

guess_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    "csv" = "text/csv",
    "tsv" = "text/tab-separated-values",
    "json" = "application/json",
    "png" = "image/png",
    "jpg" = , "jpeg" = "image/jpeg",
    "pdf" = "application/pdf",
    "rds" = "application/octet-stream",
    "html" = "text/html",
    "txt" = "text/plain",
    "application/octet-stream")
}

#' Run the full HTTP entry point
#'
#' Starts the static server (if not already running) and serves the
#' MCP server over Streamable HTTP. Daemons are sized via the
#' RCPA_DAEMONS env var (default 4). Blocks until shutdown.
#'
#' @param port MCP port (default: env RCPA_PORT or 9004).
#' @param static_port Static server port (default: env RCPA_STATIC_PORT or 9005).
#' @param daemons Mirai daemon count (default: env RCPA_DAEMONS or 4).
#' @param allowed_origins Origins for CORS / Origin header validation.
#' @param ... Forwarded to mcpserver::serve_http.
#' @export
run_http_entrypoint <- function(port = NULL,
                                static_port = NULL,
                                daemons = NULL,
                                allowed_origins = NULL, ...) {
  port <- as.integer(port %||%
                       Sys.getenv("RCPA_PORT", unset = "9004"))
  static_port <- as.integer(static_port %||%
                               Sys.getenv("RCPA_STATIC_PORT", unset = "9005"))
  daemons <- as.integer(daemons %||%
                           Sys.getenv("RCPA_DAEMONS", unset = "4"))
  allowed_origins <- allowed_origins %||%
    c("http://localhost", sprintf("http://localhost:%d", port),
      "http://127.0.0.1", sprintf("http://127.0.0.1:%d", port))

  mirai::daemons(daemons)
  on.exit(mirai::daemons(0L), add = TRUE)
  mirai::everywhere(do.call(
    "suppressPackageStartupMessages",
    list(expr = quote({
      do.call("library", list("mcpserver"))
      do.call("library", list("rcpa.mcpserver"))
    }))
  ))

  static <- spawn_static_server(port = static_port)
  message(sprintf("[Info] Static server listening on %s:%d (results dir: %s)",
                  Sys.getenv("RCPA_STATIC_HOST", unset = "127.0.0.1"),
                  static_port, results_dir()))
  message(sprintf("[Info] MCP server listening on /mcp (port %d)", port))

  srv <- build_rcpa_server()
  mcpserver::serve_http(srv, port = port,
                        host = Sys.getenv("RCPA_HOST", unset = "0.0.0.0"),
                        path = "/mcp",
                        allowed_origins = allowed_origins,
                        require_origin = FALSE,
                        daemons = daemons,
                        ...)
}
