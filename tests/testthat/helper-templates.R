# Helpers for Tier-3 template smoke tests.
# Tier-3 tests exercise the actual scientific R code inside each template
# by spawning a real Rscript subprocess. They need RCPA + Bioconductor
# installed.

skip_if_no_rcpa <- function() {
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_TEMPLATE_TESTS")),
    "RCPA_RUN_TEMPLATE_TESTS not set"
  )
  testthat::skip_if_not_installed("RCPA")
  testthat::skip_if_not_installed("SummarizedExperiment")
  testthat::skip_if_not_installed("limma")
}

skip_if_no_kegg <- function() {
  testthat::skip_if(
    !nzchar(Sys.getenv("RCPA_RUN_SPIA_TESTS")),
    "RCPA_RUN_SPIA_TESTS not set (skips tests requiring KEGG network downloads)"
  )
}

# Path to a fixture installed under inst/fixtures/.
fixture_path <- function(name) {
  p <- system.file("fixtures", name, package = "rcpa.mcpserver")
  if (!nzchar(p) || !file.exists(p)) {
    p <- file.path("inst", "fixtures", name)
  }
  if (!file.exists(p)) {
    stop(sprintf("fixture '%s' not found", name))
  }
  p
}

# Run a template via the real make_job_script + Rscript subprocess path.
# Returns the run result alongside the run directory so tests can inspect
# both the subprocess output and the on-disk artifacts.
run_template <- function(template, params, timeout = 300) {
  run <- make_run_dir()
  job <- make_job_script(run$dir, "smoke", template, params)
  start <- Sys.time()
  res <- run_job_sync(job$script_path, "smoke")
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (elapsed > timeout) {
    warning(sprintf("template '%s' exceeded %ds budget (took %.1fs)",
                    template, timeout, elapsed))
  }
  list(run_id = run$run_id,
       dir = run$dir,
       script = job$script_path,
       result = res,
       elapsed = elapsed)
}

# Build an experiment-design JSON for the DE template (matches what
# tool_de_analysis writes).
write_design_json <- function(sample_names, groups = NULL, pairs = NULL) {
  if (is.null(groups)) {
    half <- length(sample_names) %/% 2L
    groups <- c(rep("Control",   half),
                rep("Treatment", length(sample_names) - half))
  }
  rows <- lapply(seq_along(sample_names), function(i) {
    entry <- list(sample = sample_names[[i]], group = groups[[i]])
    if (!is.null(pairs)) entry$pair <- pairs[[i]]
    entry
  })
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(rows, tmp, auto_unbox = TRUE)
  tmp
}

# Check the first 8 bytes of a file against the PNG magic number.
is_valid_png <- function(path) {
  if (!file.exists(path)) return(FALSE)
  if (file.info(path)$size < 8L) return(FALSE)
  hdr <- readBin(path, what = "raw", n = 8L)
  identical(hdr, as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))
}
