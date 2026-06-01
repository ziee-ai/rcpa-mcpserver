#' Run the RCPA MCP server over stdio
#'
#' Serves the RCPA MCP server over the stdio transport by delegating to
#' \code{mcpserver::serve_io()}: newline-delimited JSON-RPC is read from
#' \code{stdin} and responses are written to \code{stdout}. Mirrors the daemon
#' setup of \code{\link{run_http_entrypoint}} (sizes the \code{mirai} daemon
#' pool and loads \code{mcpserver} + \code{rcpa.mcpserver} into the daemons so
#' tool handlers resolve), then blocks until \code{stdin} closes.
#'
#' Result delivery is selectable:
#' \itemize{
#'   \item \code{"file"} (default): tools emit \code{file://} URIs pointing at
#'     the on-disk results directory and no HTTP server is started. Suited to a
#'     local stdio client that reads results off disk.
#'   \item \code{"http"}: the read-only static results server is started and
#'     tools emit \code{http://} links, matching the HTTP transport.
#' }
#'
#' \code{stdout} is reserved for the MCP protocol; all diagnostics go to
#' \code{stderr} (or \code{log_path}).
#'
#' @param results Result delivery mode, \code{"file"} or \code{"http"}.
#'   Defaults to env var \code{RCPA_RESULTS_MODE}, then \code{"file"}.
#' @param daemons Mirai daemon count (default env \code{RCPA_DAEMONS} or 4).
#' @param static_port Static server port when \code{results = "http"}
#'   (default env \code{RCPA_STATIC_PORT} or 9005).
#' @param log_path Optional file to redirect stderr/log to (default env
#'   \code{RCPA_LOG}; \code{NULL} keeps the inherited stderr).
#' @return \code{NULL}, invisibly. Blocks until stdin closes.
#' @export
start_stdio_server <- function(results = NULL,
                               daemons = NULL,
                               static_port = NULL,
                               log_path = NULL) {
  results <- match.arg(
    results %||% Sys.getenv("RCPA_RESULTS_MODE", unset = "file"),
    c("file", "http"))
  daemons <- as.integer(daemons %||% Sys.getenv("RCPA_DAEMONS", unset = "4"))
  log_path <- log_path %||% {
    v <- Sys.getenv("RCPA_LOG", unset = "")
    if (nzchar(v)) v else NULL
  }

  # Make the chosen mode visible to result_uri(), including inside daemons
  # spawned below (which inherit the parent process environment).
  Sys.setenv(RCPA_RESULTS_MODE = results)

  mirai::daemons(daemons)
  on.exit(mirai::daemons(0L), add = TRUE)
  mirai::everywhere(do.call(
    "suppressPackageStartupMessages",
    list(expr = quote({
      do.call("library", list("mcpserver"))
      do.call("library", list("rcpa.mcpserver"))
    }))
  ))

  if (identical(results, "http")) {
    spawn_static_server(port = static_port)
    message(sprintf(
      "[Info] Static server on %s:%s (results dir: %s)",
      Sys.getenv("RCPA_STATIC_HOST", unset = "127.0.0.1"),
      static_port %||% Sys.getenv("RCPA_STATIC_PORT", unset = "9005"),
      results_dir()))
  } else {
    message(sprintf("[Info] Results delivered as local file:// paths under %s",
                    results_dir()))
  }
  message("[Info] MCP server on stdio (newline-delimited JSON-RPC)")

  srv <- build_rcpa_server()
  mcpserver::serve_io(srv, log_path = log_path, daemons = daemons)
}
