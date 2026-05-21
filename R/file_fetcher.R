fetch_to_tempfile <- function(uri, suffix = ".csv", timeout = 60) {
  resolved <- rewrite_local_uri(uri)
  # Local file:// URIs are accepted only when explicitly allowed
  # (test environment) — produces a temp copy so callers can unlink()
  # without affecting the source file.
  if (is_local_file_uri(resolved) && allow_local_uris()) {
    src <- sub("^file://", "", resolved)
    tmp <- tempfile(fileext = suffix)
    if (!file.copy(src, tmp, overwrite = TRUE)) {
      stop(sprintf("Failed to copy local file %s", src))
    }
    return(tmp)
  }
  if (!is_safe_uri(resolved)) {
    stop(sprintf("Refusing to fetch URI '%s': only http(s) URLs are allowed",
                 resolved))
  }
  tmp <- tempfile(fileext = suffix)
  tryCatch(
    httr2::req_perform(
      httr2::req_timeout(httr2::request(resolved), timeout),
      path = tmp
    ),
    error = function(e) {
      stop(sprintf("Failed to fetch %s: %s", resolved, conditionMessage(e)))
    }
  )
  tmp
}

is_safe_uri <- function(uri) {
  if (!is.character(uri) || length(uri) != 1L) return(FALSE)
  grepl("^https?://", uri, ignore.case = TRUE) &&
    !grepl("^file://", uri, ignore.case = TRUE) &&
    !grepl("^data:", uri, ignore.case = TRUE)
}

is_local_file_uri <- function(uri) {
  is.character(uri) && length(uri) == 1L &&
    grepl("^file://", uri, ignore.case = TRUE)
}

allow_local_uris <- function() {
  isTRUE(as.logical(Sys.getenv("RCPA_ALLOW_LOCAL_URIS", unset = "FALSE")))
}

# Coder port-forward URL rewriter kept for old-environment compatibility.
# Only triggers when the user explicitly opts in via RCPA_CODER_HOST.
rewrite_local_uri <- function(uri) {
  host_tpl <- Sys.getenv("RCPA_CODER_HOST", unset = "")
  if (!nzchar(host_tpl)) return(uri)
  m <- regmatches(
    uri,
    regexec("^https?://(?:127\\.0\\.0\\.1|localhost):([0-9]+)(.*)",
            uri, perl = TRUE)
  )[[1]]
  if (length(m) == 0L) return(uri)
  port <- m[[2]]
  path <- m[[3]]
  paste0("https://", port, "--", host_tpl, path)
}
