#!/usr/bin/env Rscript
## Launch rcpa.mcpserver over Streamable HTTP.
##
## Standard env vars: RCPA_PORT, RCPA_HOST, RCPA_STATIC_PORT,
##   RCPA_STATIC_HOST, RCPA_DAEMONS, RCPA_RESULTS_DIR, BASE_URL, RCPA_LOG.
##
## Optional auth (handled by R/auth_config.R; defaults to off):
##   RCPA_AUTH=on              enable JWT auth + admin REST + admin SPA
##   MCPSERVER_ADMIN_TOKEN     bootstrap admin token (auto-generated if unset)
##   RCPA_AUTH_DB              SQLite path (default <results_dir>/auth.db)
##   RCPA_AUTH_ISSUER          JWT iss claim (default http://127.0.0.1:<port>)
##   RCPA_AUTH_AUDIENCE        JWT aud claim (default "rcpa")
##   RCPA_AUTH_UI              "off" to hide the bundled /admin/ui SPA

log_path <- Sys.getenv("RCPA_LOG", unset = "")
if (nzchar(log_path)) {
  sink(file(log_path, open = "a"), type = "message")
}

tryCatch({
  suppressPackageStartupMessages(library(rcpa.mcpserver))
  args <- commandArgs(trailingOnly = TRUE)
  port <- as.integer(Sys.getenv("RCPA_PORT", unset = "9004"))
  i <- match("--port", args)
  if (!is.na(i) && i < length(args)) port <- as.integer(args[[i + 1L]])
  run_http_entrypoint(port = port)
}, error = function(e) {
  message("run-http.R fatal: ", conditionMessage(e))
  quit(status = 1L, save = "no")
})
