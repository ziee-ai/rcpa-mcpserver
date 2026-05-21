`%||%` <- function(a, b) if (!is.null(a)) a else b

.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .pkg_env$results_dir <- Sys.getenv("RCPA_RESULTS_DIR",
                                     unset = file.path(tempdir(), "rcpa-results"))
  .pkg_env$base_url <- Sys.getenv("BASE_URL",
                                   unset = "http://localhost:9005")
  .pkg_env$rscript <- file.path(R.home("bin"), "Rscript")
  dir.create(.pkg_env$results_dir, recursive = TRUE, showWarnings = FALSE)
  invisible()
}

results_dir <- function() .pkg_env$results_dir
base_url <- function() .pkg_env$base_url
rscript_path <- function() .pkg_env$rscript

template_path <- function(name) {
  p <- system.file("templates", paste0(name, ".R"), package = "rcpa.mcpserver")
  if (!nzchar(p) || !file.exists(p)) {
    p <- file.path("inst", "templates", paste0(name, ".R"))
  }
  p
}
