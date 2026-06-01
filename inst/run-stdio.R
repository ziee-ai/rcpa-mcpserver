#!/usr/bin/env Rscript
## Launch rcpa.mcpserver over stdio (newline-delimited JSON-RPC on stdin/stdout).
##
## Env vars: RCPA_RESULTS_MODE (file|http, default file), RCPA_DAEMONS,
##   RCPA_RESULTS_DIR, RCPA_LOG, and (when RCPA_RESULTS_MODE=http)
##   RCPA_STATIC_PORT, RCPA_STATIC_HOST, BASE_URL.
##
## stdout is reserved for the MCP protocol; diagnostics go to stderr (or RCPA_LOG).

tryCatch({
  suppressPackageStartupMessages(library(rcpa.mcpserver))
  start_stdio_server()
}, error = function(e) {
  message("run-stdio.R fatal: ", conditionMessage(e))
  quit(status = 1L, save = "no")
})
