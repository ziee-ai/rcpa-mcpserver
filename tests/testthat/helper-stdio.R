# Full stdio integration: spawn start_stdio_server() in a child Rscript and
# drive it over stdin/stdout. Mirrors mcpserver's own stdio test harness
# (mcpserver-r/tests/testthat/test-stdio.R) — the read loop runs on the child's
# main thread, so an in-process call would deadlock it.

skip_if_no_stdio_deps <- function() {
  testthat::skip_if_not_installed("processx")
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_on_cran()
}

# Build a tempfile R script that loads the package and starts the stdio server.
# Optional `extra` is a character vector of additional lines (e.g. Sys.setenv)
# emitted before start_stdio_server().
spawn_rcpa_stdio <- function(results = "file", daemons = 2L,
                             allow_local = TRUE, results_dir = NULL,
                             extra = character(0L),
                             startup_wait = 3) {
  runner_script <- tempfile(fileext = ".R")
  writeLines(c(
    "suppressPackageStartupMessages(library(mcpserver))",
    "suppressPackageStartupMessages(library(rcpa.mcpserver))",
    extra,
    sprintf("start_stdio_server(results = %s, daemons = %dL)",
            shQuote(results), as.integer(daemons))
  ), runner_script)

  child_env <- Sys.getenv()
  child_env["R_LIBS"] <- paste(.libPaths(), collapse = .Platform$path.sep)
  child_env["RCPA_ALLOW_LOCAL_URIS"] <- if (isTRUE(allow_local)) "TRUE" else "FALSE"
  if (!is.null(results_dir)) {
    dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
    results_dir <- normalizePath(results_dir, mustWork = FALSE)
    child_env["RCPA_RESULTS_DIR"] <- results_dir
  }

  p <- processx::process$new("Rscript", c(runner_script),
                             stdin = "|", stdout = "|", stderr = "|",
                             env = child_env)
  Sys.sleep(startup_wait)
  if (!p$is_alive()) {
    err <- tryCatch(p$read_all_error(), error = function(e) "")
    stop(sprintf("stdio server failed to start: stderr=%s", err))
  }

  buf <- new.env(parent = emptyenv())
  buf$lines <- character(0L)
  buf$partial <- ""
  list(process = p, runner_script = runner_script,
       results_dir = results_dir, buf = buf)
}

stop_rcpa_stdio <- function(server) {
  tryCatch(server$process$kill(), error = function(e) NULL)
  tryCatch(unlink(server$runner_script), error = function(e) NULL)
}

# Encode and write one JSON-RPC message (as a named list) to the child's stdin.
send_msg <- function(server, msg) {
  line <- paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n")
  server$process$write_input(line)
  invisible()
}

# Write a raw line (already-formed JSON or deliberately malformed) to stdin.
send_raw <- function(server, raw_line) {
  server$process$write_input(paste0(raw_line, "\n"))
  invisible()
}

# Buffered line reader copied from mcpserver-r/tests/testthat/test-stdio.R:
# a persistent buffer keeps queued lines when the child flushes several
# replies (e.g. a batch) at once.
read_line <- function(server, timeout_ms = 20000) {
  buf <- server$buf
  if (length(buf$lines) > 0L) {
    out <- buf$lines[[1L]]
    buf$lines <- buf$lines[-1L]
    return(out)
  }
  p <- server$process
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < timeout_ms / 1000) {
    p$poll_io(200)
    chunk <- p$read_output()
    if (nchar(chunk) > 0L) {
      buf$partial <- paste0(buf$partial, chunk)
      if (grepl("\n", buf$partial, fixed = TRUE)) {
        parts <- strsplit(buf$partial, "\n", fixed = TRUE)[[1L]]
        if (endsWith(buf$partial, "\n")) {
          buf$lines <- c(buf$lines, parts)
          buf$partial <- ""
        } else {
          buf$lines <- c(buf$lines, parts[-length(parts)])
          buf$partial <- parts[[length(parts)]]
        }
        buf$lines <- buf$lines[nzchar(buf$lines)]
        if (length(buf$lines) > 0L) {
          out <- buf$lines[[1L]]
          buf$lines <- buf$lines[-1L]
          return(out)
        }
      }
    }
    if (!p$is_alive()) break
  }
  NA_character_
}

# Read the next JSON-RPC envelope, parsed. Errors (with the child's stderr)
# if nothing arrives before the timeout.
read_msg <- function(server, timeout_ms = 20000) {
  line <- read_line(server, timeout_ms)
  if (is.na(line)) {
    stop(sprintf("timeout/no response; stderr: %s",
                 paste(tryCatch(server$process$read_error_lines(),
                                error = function(e) ""),
                       collapse = " | ")))
  }
  jsonlite::fromJSON(line, simplifyVector = FALSE)
}

# Perform the initialize handshake with NO elicitation capability (so the
# heavy tools fall back to their defaults instead of prompting). Returns the
# parsed initialize result envelope.
stdio_initialize <- function(server, timeout_ms = 20000) {
  send_msg(server, list(jsonrpc = "2.0", id = 1L, method = "initialize",
                        params = list(protocolVersion = "2025-06-18",
                                      clientInfo = list(name = "test",
                                                        version = "0"),
                                      capabilities = list())))
  init <- read_msg(server, timeout_ms)
  send_msg(server, list(jsonrpc = "2.0", method = "notifications/initialized"))
  init
}

# Send a tools/call and return the parsed response envelope.
stdio_call_tool <- function(server, id, name, arguments = list(),
                            timeout_ms = 30000) {
  send_msg(server, list(jsonrpc = "2.0", id = id, method = "tools/call",
                        params = list(name = name, arguments = arguments)))
  read_msg(server, timeout_ms)
}

# First text content item of a tool result.
result_text <- function(resp) {
  for (item in resp$result$content %||% list()) {
    if (identical(item$type, "text") && !is.null(item$text)) return(item$text)
  }
  ""
}

# On-disk path from the resource_link content item (file:// stripped).
link_path <- function(resp) {
  for (item in resp$result$content %||% list()) {
    if (identical(item$type, "resource_link") && !is.null(item$uri)) {
      return(sub("^file://", "", item$uri))
    }
  }
  NA_character_
}

# Resource link URI (unstripped) — preserves http:// or file:// scheme.
link_uri <- function(resp) {
  for (item in resp$result$content %||% list()) {
    if (identical(item$type, "resource_link") && !is.null(item$uri)) {
      return(item$uri)
    }
  }
  NA_character_
}
