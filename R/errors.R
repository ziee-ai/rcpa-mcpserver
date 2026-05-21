# Tool-level error response shaped for mcpserver::response_error.
# Returns a list with isError=TRUE. The wire envelope is success;
# only the inner content carries the error flag (matches mcpserver
# conventions and the old server's mcp_error() semantics).
mcp_tool_error <- function(msg, ...) {
  details <- list(...)
  text <- if (length(details) > 0L) {
    paste0(
      paste(msg, collapse = "\n"), "\n",
      paste(names(details),
            vapply(details, function(d) paste(as.character(d), collapse = ", "),
                   character(1L)),
            sep = ": ", collapse = "\n")
    )
  } else {
    paste(msg, collapse = "\n")
  }
  mcpserver::response_error(text)
}

is_tool_error <- function(x) {
  is.list(x) && isTRUE(x$isError)
}

# Safe predicate for "this entry is a real on-disk file path".
# Tool prepare paths collect file references but may also hold error
# conditions from failed tryCatch's; file.exists() chokes on those.
is_path <- function(p) {
  !is.null(p) && is.character(p) && length(p) == 1L && nzchar(p)
}

safe_unlink_all <- function(paths) {
  for (p in paths) {
    if (is_path(p) && file.exists(p)) unlink(p)
  }
  invisible()
}
