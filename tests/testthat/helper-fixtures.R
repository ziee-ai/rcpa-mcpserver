file_uri <- function(path) {
  paste0("file://", normalizePath(path, mustWork = FALSE))
}

# Tests load via load_all so tests/testthat.R doesn't run; set the
# opt-in here as well for safety.
Sys.setenv(RCPA_ALLOW_LOCAL_URIS = "TRUE")

write_expr_csv <- function(n_genes = 20L, n_samples = 10L,
                           sample_names = paste0("sample", seq_len(n_samples)),
                           gene_prefix = "gene") {
  tmp <- tempfile(fileext = ".csv")
  m <- matrix(stats::rnorm(n_genes * n_samples, mean = 5, sd = 1),
              nrow = n_genes, ncol = n_samples,
              dimnames = list(paste0(gene_prefix, seq_len(n_genes)),
                              sample_names))
  utils::write.csv(m, tmp, row.names = TRUE)
  tmp
}

write_design_csv <- function(sample_names,
                             groups = NULL,
                             pairs = NULL) {
  if (is.null(groups)) {
    half <- length(sample_names) %/% 2L
    groups <- c(rep("Control",   half),
                rep("Treatment", length(sample_names) - half))
  }
  df <- data.frame(sample = sample_names,
                   group = groups,
                   stringsAsFactors = FALSE)
  if (!is.null(pairs)) df$pair <- pairs
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)
  tmp
}

write_gene_stats_csv <- function(n = 50L,
                                 cols = c("id", "logFC", "pvalue")) {
  df <- data.frame(
    id     = paste0("gene", seq_len(n)),
    logFC  = stats::rnorm(n),
    pvalue = stats::runif(n)
  )
  df <- df[, cols, drop = FALSE]
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(df, tmp, row.names = FALSE)
  tmp
}

# Mock McpCtx: stubs out everything tool handlers might call.
# request_elicitation returns canned values from `elicit_returns`
# (a list of `list(content = ...)` objects, consumed in order).
build_mock_ctx <- function(elicit_returns = list(),
                           cancelled = FALSE) {
  e <- new.env(parent = emptyenv())
  e$session_id <- "test-session"
  e$client_capabilities <- list(elicitation = list())
  e$auth_subject <- NULL
  e$auth_scopes <- NULL
  e$progress_token <- NULL
  e$msg_meta <- NULL
  e$.elicit_queue <- elicit_returns
  e$.elicit_calls <- list()
  e$.cancel <- cancelled
  e$.logs <- list()
  e$send_log <- function(level, message, logger = NULL, data = NULL) {
    e$.logs[[length(e$.logs) + 1L]] <-
      list(level = level, message = message, logger = logger, data = data)
    invisible()
  }
  e$send_progress <- function(progress, total = NULL, message = NULL) invisible()
  e$cancelled <- function() isTRUE(e$.cancel)
  e$on_cancel <- function(fn) invisible()
  e$request_elicitation <- function(message, requested_schema, timeout = 30) {
    e$.elicit_calls[[length(e$.elicit_calls) + 1L]] <-
      list(message = message, schema = requested_schema)
    if (length(e$.elicit_queue) == 0L) {
      stop("mock ctx: no elicitation response queued")
    }
    resp <- e$.elicit_queue[[1L]]
    e$.elicit_queue <- e$.elicit_queue[-1L]
    resp
  }
  # NOTE: deliberately NOT setting class to McpCtx - the framework's
  # $.McpCtx S3 method intercepts request_elicitation and routes it to
  # the real implementation, which would defeat the mock. A plain
  # environment uses normal $ access and returns the assigned function.
  e
}

error_text <- function(err) {
  if (!is.list(err)) return("")
  if (!is.null(err$content) && is.list(err$content)) {
    return(paste(vapply(err$content, function(c) c$text %||% "",
                        character(1L)),
                 collapse = "\n"))
  }
  ""
}
