#!/usr/bin/env Rscript
## Launch rcpa.mcpserver over Streamable HTTP.

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
