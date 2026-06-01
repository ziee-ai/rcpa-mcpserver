# All 6 RCPA tools exercised end-to-end over the stdio transport, in file
# results mode (file:// links resolved on disk). Tier-3 gated exactly like
# test-http-integration.R's heavy test and the template smoke tests:
#   - skip_if_no_rcpa()  -> RCPA_RUN_TEMPLATE_TESTS + RCPA/limma installed
#   - KEGG paths also gate on RCPA_RUN_PA_TESTS (first KEGG catalogue fetch
#     may hit the network), mirroring test-template-pa/consensus/meta.
# validate_input_file is covered (always-on) in test-stdio-protocol.R.
#
# Argument shapes and method choices here mirror the validated contracts in
# test-{de,pa,consensus,meta,plot}-prepare.R and test-template-*.R so the
# tool-facing JSON-RPC calls drive the same code paths those tests construct.

# KEGG pathway paths need an opt-in beyond RCPA_RUN_TEMPLATE_TESTS because the
# first RCPA::getGeneSets("KEGG") call may download the catalogue.
skip_if_no_pa_network <- function() {
  skip_if_no_rcpa()
  testthat::skip_if(!nzchar(Sys.getenv("RCPA_RUN_PA_TESTS")),
                    "RCPA_RUN_PA_TESTS not set")
}

# ── Differential expression ──────────────────────────────────────────────

test_that("stdio run_de_analysis (limma) produces a DE CSV end-to-end", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_de_analysis",
    list(expression_uri        = file_uri(fixture_path("small_expr.csv")),
         experiment_design_uri = file_uri(fixture_path("small_design.csv")),
         method                = "limma",
         contrast              = "Treatment - Control"),
    timeout_ms = 120000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"de_result"')

  uri <- link_uri(resp)
  expect_false(is.na(uri))
  expect_match(uri, "^file://", info = "stdio file mode must emit file:// URIs")
  p <- link_path(resp)
  expect_true(file.exists(p), info = p)
  hdr <- readLines(p, n = 1L)
  expect_match(hdr, "logFC", ignore.case = TRUE)
})

test_that("stdio run_de_analysis (DESeq2) produces a DE CSV end-to-end", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  testthat::skip_if_not_installed("DESeq2")
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_de_analysis",
    list(expression_uri        = file_uri(fixture_path("small_expr_counts.csv")),
         experiment_design_uri = file_uri(fixture_path("small_design.csv")),
         method                = "DESeq2",
         contrast              = "Treatment - Control"),
    timeout_ms = 180000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"de_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

test_that("stdio run_de_analysis (edgeR) produces a DE CSV end-to-end", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  testthat::skip_if_not_installed("edgeR")
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_de_analysis",
    list(expression_uri        = file_uri(fixture_path("small_expr_counts.csv")),
         experiment_design_uri = file_uri(fixture_path("small_design.csv")),
         method                = "edgeR",
         contrast              = "Treatment - Control"),
    timeout_ms = 180000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"de_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

# ── Pathway analysis ─────────────────────────────────────────────────────

test_that("stdio run_pathway_analysis (ora + KEGG) emits a PA CSV", {
  skip_if_no_stdio_deps()
  skip_if_no_pa_network()
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_pathway_analysis",
    list(gene_stats_uri = file_uri(fixture_path("small_gene_stats.csv")),
         databases      = list("KEGG"),
         methods        = list("ora"),
         org            = "hsa",
         namespace      = "biological_process"),
    timeout_ms = 300000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"pa_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

# ── Consensus (>= 2 PA methods on one dataset, run internally) ────────────

test_that("stdio run_consensus_analysis combines ora + fgsea", {
  skip_if_no_stdio_deps()
  skip_if_no_pa_network()
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_consensus_analysis",
    list(gene_stats_uri = file_uri(fixture_path("small_gene_stats.csv")),
         database       = "KEGG",
         pa_methods     = list("ora", "fgsea"),
         org            = "hsa",
         namespace      = "biological_process",
         method         = "weightedZMean",
         use_fdr        = TRUE,
         rank_by        = "normalizedScore"),
    timeout_ms = 600000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"consensus_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

# ── Meta-analysis (DE mode: raw inputs per study, DE run internally) ──────

test_that("stdio run_meta_analysis (DE meta) combines 2 studies", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  expr_uri <- file_uri(fixture_path("small_expr.csv"))
  design_uri <- file_uri(fixture_path("small_design.csv"))
  studies <- list(
    list(expression_uri = expr_uri, experiment_design_uri = design_uri,
         contrast = "Treatment - Control"),
    list(expression_uri = expr_uri, experiment_design_uri = design_uri,
         contrast = "Treatment - Control")
  )

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_meta_analysis",
    list(studies   = studies,
         de_method = "limma",
         methods   = list("fisher")),
    timeout_ms = 300000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"meta_de_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

# ── Meta-analysis (PA mode: per-study gene stats, same PA method each) ────

test_that("stdio run_meta_analysis (PA meta) combines 2 gene-stats studies", {
  skip_if_no_stdio_deps()
  skip_if_no_pa_network()
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  resp <- stdio_call_tool(srv, 2L, "run_meta_analysis",
    list(gene_stats_uris = list(
           file_uri(fixture_path("small_gene_stats.csv")),
           file_uri(fixture_path("small_gene_stats_alt.csv"))),
         database  = "KEGG",
         pa_method = "ora",
         org       = "hsa",
         namespace = "biological_process",
         methods   = list("fisher")),
    timeout_ms = 600000)
  expect_false(isTRUE(resp$result$isError), info = result_text(resp))
  expect_match(result_text(resp), '"result_type":"meta_pa_result"')
  expect_match(link_uri(resp), "^file://")
  expect_true(file.exists(link_path(resp)))
})

# ── Plotting (volcano + heatmap on a DE result, fed back as file:// URI) ──

test_that("stdio plot_results (volcano on a DE CSV) emits a PNG", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  testthat::skip_if_not_installed("ggplot2")
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  de_resp <- stdio_call_tool(srv, 2L, "run_de_analysis",
    list(expression_uri        = file_uri(fixture_path("small_expr.csv")),
         experiment_design_uri = file_uri(fixture_path("small_design.csv")),
         method                = "limma",
         contrast              = "Treatment - Control"),
    timeout_ms = 180000)
  expect_false(isTRUE(de_resp$result$isError), info = result_text(de_resp))
  de_uri <- link_uri(de_resp)
  expect_match(de_uri, "^file://")

  plot_resp <- stdio_call_tool(srv, 3L, "plot_results",
    list(result_uri  = de_uri,
         result_type = "de_result",
         type        = "volcano"),
    timeout_ms = 120000)
  expect_false(isTRUE(plot_resp$result$isError),
               info = result_text(plot_resp))
  expect_match(link_uri(plot_resp), "^file://")
  png_path <- link_path(plot_resp)
  expect_true(file.exists(png_path))
  expect_true(is_valid_png(png_path),
              info = paste("not a valid PNG:", png_path))
})

test_that("stdio plot_results (heatmap on a DE CSV) emits a PNG", {
  skip_if_no_stdio_deps()
  skip_if_no_rcpa()
  testthat::skip_if_not_installed("ggplot2")
  rdir <- withr::local_tempdir()
  srv <- spawn_rcpa_stdio(results = "file", results_dir = rdir,
                          startup_wait = 5)
  withr::defer(stop_rcpa_stdio(srv))

  stdio_initialize(srv)
  de_resp <- stdio_call_tool(srv, 2L, "run_de_analysis",
    list(expression_uri        = file_uri(fixture_path("small_expr.csv")),
         experiment_design_uri = file_uri(fixture_path("small_design.csv")),
         method                = "limma",
         contrast              = "Treatment - Control"),
    timeout_ms = 180000)
  expect_false(isTRUE(de_resp$result$isError), info = result_text(de_resp))
  de_uri <- link_uri(de_resp)

  plot_resp <- stdio_call_tool(srv, 3L, "plot_results",
    list(result_uri  = de_uri,
         result_type = "de_result",
         type        = "heatmap",
         top_n_genes = 10L),
    timeout_ms = 120000)
  expect_false(isTRUE(plot_resp$result$isError),
               info = result_text(plot_resp))
  expect_match(link_uri(plot_resp), "^file://")
  expect_true(is_valid_png(link_path(plot_resp)))
})
