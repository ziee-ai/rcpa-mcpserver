# Generate the 3-line job script that loads params.json and sources the
# template inside a fresh Rscript subprocess. Matches the audit-trail
# pattern from the old server: results/{run_id}/{job}_params.json +
# results/{run_id}/{job}.R can be re-run by hand for reproducibility.
make_job_script <- function(out_dir, job_name, template_name, params) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  params_file <- file.path(out_dir, paste0(job_name, "_params.json"))
  script_file <- file.path(out_dir, paste0(job_name, ".R"))
  tpl_path <- template_path(template_name)
  if (!file.exists(tpl_path)) {
    stop(sprintf("template '%s' not found at %s", template_name, tpl_path))
  }
  jsonlite::write_json(params, params_file, auto_unbox = TRUE, null = "null")
  writeLines(c(
    "library(jsonlite)",
    sprintf('params <- jsonlite::fromJSON("%s", simplifyVector = FALSE)',
            params_file),
    sprintf('source("%s")', tpl_path)
  ), script_file)
  list(script_path = script_file, params_path = params_file)
}

# Run a job script asynchronously by queueing it to a mirai daemon. Inside
# the daemon, processx spawns a fresh Rscript subprocess so the analysis
# gets full process isolation. Returns a promise that resolves to
# list(success, stdout, stderr, exit_code, job_name).
run_job_async <- function(script_path, job_name,
                          rscript = rscript_path(),
                          cancel_flag_path = NULL) {
  mirai_obj <- mirai::mirai(
    {
      p <- processx::process$new(rscript,
                                 c("--vanilla", script_path),
                                 stdout = "|", stderr = "|")
      while (p$is_alive()) {
        if (!is.null(cancel_flag_path) &&
            nzchar(cancel_flag_path) &&
            file.exists(cancel_flag_path)) {
          p$kill()
          break
        }
        Sys.sleep(0.1)
      }
      out <- tryCatch(p$read_all_output(), error = function(e) "")
      err <- tryCatch(p$read_all_error(),  error = function(e) "")
      list(
        job_name  = job_name,
        success   = identical(p$get_exit_status(), 0L),
        stdout    = out,
        stderr    = err,
        exit_code = p$get_exit_status() %||% -1L
      )
    },
    rscript = rscript,
    script_path = script_path,
    job_name = job_name,
    cancel_flag_path = cancel_flag_path %||% ""
  )
  promises::as.promise(mirai_obj)
}

# Synchronous variant for the synchronous-handler path (used when the
# tool runs inside a daemon already and we want to block). The function
# spawns processx, streams to messages, and returns the same shape.
run_job_sync <- function(script_path, job_name,
                         rscript = rscript_path()) {
  p <- processx::process$new(rscript,
                             c("--vanilla", script_path),
                             stdout = "|", stderr = "|")
  while (p$is_alive()) Sys.sleep(0.05)
  list(
    job_name  = job_name,
    success   = identical(p$get_exit_status(), 0L),
    stdout    = tryCatch(p$read_all_output(), error = function(e) ""),
    stderr    = tryCatch(p$read_all_error(),  error = function(e) ""),
    exit_code = p$get_exit_status() %||% -1L
  )
}

# Build a unique run directory: results/{YYYYMMDD_HHMMSS}_{nnnn}/
make_run_dir <- function(base = results_dir()) {
  run_id <- sprintf("%s_%04d",
                    format(Sys.time(), "%Y%m%d_%H%M%S"),
                    sample.int(9999L, 1L))
  d <- file.path(base, run_id)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  list(run_id = run_id, dir = d)
}
