# Auth bootstrap glue between rcpa-mcpserver and mcpserver --------------

# This module decides, from environment variables only, whether and how
# to enable JWT auth + the admin REST API + the bundled admin SPA on
# top of the existing /mcp transport. The default is OFF so existing
# deployments keep their current behavior; flipping `RCPA_AUTH=on`
# turns the entire stack on without any code changes.
#
# The returned value is spliced into `mcpserver::serve_http(...)` by
# `run_http_entrypoint()` in R/static_server.R. When this returns NULL
# the caller forwards nothing, so serve_http() runs identically to today.

#' Build the auth-related kwargs for `mcpserver::serve_http()`
#'
#' Reads env vars and returns either `NULL` (auth disabled — current
#' default behavior) or a list containing `oauth_as` and `admin` ready
#' to be spliced into the `serve_http()` call.
#'
#' Env vars consulted:
#'
#' * `RCPA_AUTH` — `"on"` to enable; anything else (default) leaves
#'   the server unauthenticated.
#' * `MCPSERVER_ADMIN_TOKEN` — bootstrap admin token. When unset and
#'   auth is on, an opaque 32-byte token is auto-generated and logged
#'   to stderr **once**.
#' * `RCPA_AUTH_DB` — SQLite path for the users + tokens store.
#'   Defaults to `<results_dir>/auth.db`.
#' * `RCPA_AUTH_ISSUER` — JWT `iss` claim and AS issuer URL. Defaults
#'   to `http://127.0.0.1:<port>`.
#' * `RCPA_AUTH_AUDIENCE` — JWT `aud` claim. Defaults to `rcpa`.
#' * `RCPA_AUTH_UI` — `"on"` (default when auth is on) to mount the
#'   bundled `/admin/ui/*` SPA.
#'
#' @param port The port `serve_http()` will bind to (used to build the
#'   default issuer URL).
#' @return `NULL` (auth disabled) or `list(oauth_as = ..., admin = ...)`.
#' @keywords internal
rcpa_auth_config <- function(port) {
  if (!identical(tolower(Sys.getenv("RCPA_AUTH", unset = "off")), "on")) {
    return(NULL)
  }

  # SQLite driver is only required when auth is on.
  for (pkg in c("DBI", "RSQLite")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "RCPA_AUTH=on requires the '%s' R package. ",
        pkg),
        "Install it (and any of: DBI, RSQLite) and restart, e.g. ",
        "`install.packages(c('DBI','RSQLite'))`.",
        call. = FALSE)
    }
  }

  # Bootstrap token: read from env, or auto-generate and log once.
  bootstrap_token <- Sys.getenv("MCPSERVER_ADMIN_TOKEN", unset = "")
  generated <- FALSE
  if (!nzchar(bootstrap_token)) {
    bootstrap_token <- paste(openssl::rand_bytes(32L), collapse = "")
    generated <- TRUE
  }

  auth_db <- Sys.getenv("RCPA_AUTH_DB", unset = "")
  if (!nzchar(auth_db)) {
    auth_db <- file.path(results_dir(), "auth.db")
  }
  dir.create(dirname(auth_db), recursive = TRUE, showWarnings = FALSE)

  issuer <- Sys.getenv(
    "RCPA_AUTH_ISSUER",
    unset = sprintf("http://127.0.0.1:%d", as.integer(port)))
  audience <- Sys.getenv("RCPA_AUTH_AUDIENCE", unset = "rcpa")
  ui <- !identical(tolower(Sys.getenv("RCPA_AUTH_UI", unset = "on")),
                   "off")

  store <- mcpserver::new_mcp_store(driver = "sqlite", path = auth_db)
  oauth_as <- mcpserver::oauth_server_config(
    issuer   = issuer,
    audience = audience,
    store    = store
  )

  # One-time startup logging. Routed via message() so it lands on stderr
  # and inherits any RCPA_LOG sink configured by inst/run-http.R.
  if (isTRUE(generated)) {
    message("[Auth] MCPSERVER_ADMIN_TOKEN was not set; ",
            "generated an ephemeral one (will NOT survive restart): ",
            bootstrap_token)
    message("[Auth] Set MCPSERVER_ADMIN_TOKEN in your environment for ",
            "any persistent deployment.")
  }
  if (grepl("^/tmp(/|$)", auth_db)) {
    message("[Auth] WARNING: RCPA_AUTH_DB resolved to ", auth_db,
            " (under /tmp). Users and tokens will be lost on restart.")
  }
  message("[Auth] enabled; issuer=", issuer,
          " audience=", audience,
          " db=", auth_db,
          if (isTRUE(ui)) "; admin UI at /admin/ui" else "")

  list(
    oauth_as = oauth_as,
    admin = list(
      bootstrap_token = bootstrap_token,
      ui              = isTRUE(ui),
      # Cap minted token lifetimes at one year to keep the SQLite token
      # store from growing without bound on long-lived deployments.
      max_ttl         = 60L * 60L * 24L * 365L
    )
  )
}
