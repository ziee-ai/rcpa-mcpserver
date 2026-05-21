# rcpa.mcpserver

MCP server exposing the [RCPA](https://github.com/tinnlab/RCPA)
bioinformatics toolkit over Streamable HTTP. Built on the
[mcpserver](../mcpserver-r/) R framework.

Tools exposed:

| Tool | Purpose |
|---|---|
| `validate_input_file` | Sanity-check expression / gene-stats / design CSVs before analysis |
| `run_de_analysis` | Differential expression via limma / DESeq2 / edgeR |
| `run_pathway_analysis` | Pathway / gene-set enrichment (KEGG, GO; 7 methods) |
| `plot_results` | Volcano, MA, bar, heatmap, forest, venn, network, KEGG-map plots |
| `run_consensus_analysis` | Combine 2+ PA methods on one dataset |
| `run_meta_analysis` | Combine DE or PA results across 2+ studies |

## Quick start (Docker)

```sh
docker compose up --build
```

The MCP endpoint will be at `http://localhost:9004/mcp` and result
files at `http://localhost:9005/results/…/<file>.csv` (or `.png`).

## Local development

```sh
R -e 'devtools::install_local(".", dependencies = TRUE)'
R -e 'rcpa.mcpserver::run_http_entrypoint()'
```

## Configuration

| Env var | Default | Purpose |
|---|---|---|
| `RCPA_PORT` | `9004` | MCP `/mcp` listen port |
| `RCPA_HOST` | `0.0.0.0` | MCP bind host |
| `RCPA_STATIC_PORT` | `9005` | Static result server port |
| `RCPA_STATIC_HOST` | `127.0.0.1` | Static server bind host |
| `RCPA_DAEMONS` | `4` | Mirai worker count |
| `RCPA_RESULTS_DIR` | `tempdir()/rcpa-results` | Where job output lands |
| `BASE_URL` | `http://localhost:9005` | URL prefix for `resource_link`s |
| `RCPA_LOG` | unset | When set, stderr → that file |
| `RCPA_CODER_HOST` | unset | Coder port-forward hostname (legacy URI rewriter) |

## Architecture

Each analysis call:

1. Validates args (and interactively elicits missing fields from
   capable clients).
2. Fetches input URLs to temp files.
3. Generates `results/{run_id}/{job}.R` + `{job}_params.json`.
4. Spawns a fresh `Rscript --vanilla` subprocess (subprocess isolation
   from Bioconductor global state and OOMs).
5. Returns metadata text + `resource_link` URLs that the static server
   resolves to the output CSVs / RDS / PNGs.

The triple `{job}.R + params.json + outputs` left in `results/` is a
reproducibility audit trail — re-runnable with
`Rscript results/{run_id}/{job}.R`.

## Testing

```sh
# Tier 1 (unit), Tier 2 (elicitation w/ mock ctx), Tier 4 (dispatch),
# Tier 5 (HTTP integration via subprocess) — always on:
R -e 'testthat::test_local(".")'

# Tier 3 template smoke tests — exercise real RCPA via subprocess:
RCPA_RUN_TEMPLATE_TESTS=1 R -e 'testthat::test_local(".")'

# Add KEGG-network PA / consensus / meta-PA tests (requires internet):
RCPA_RUN_TEMPLATE_TESTS=1 RCPA_RUN_PA_TESTS=1 \
  R -e 'testthat::test_local(".")'
```

Expected: **366 passed, 0 failed, 6 skipped** (DESeq2 fixture-too-small
+ a few opt-in PA tests).

### Docker smoke test

```sh
docker build -t rcpa-mcpserver:test .
bash tests/protocol/docker-smoke.sh rcpa-mcpserver:test
```

The smoke test verifies, against a live container:

1. `initialize` returns `serverInfo.name = rcpa-mcpserver`
2. `tools/list` lists all 6 tools
3. `validate_input_file` over a tiny CSV in `file:///tmp/`
4. Static server returns files from `/results/`
5. `run_de_analysis` runs real limma DE through RCPA
6. The resulting CSV downloads via the static server with `logFC` +
   `pFDR` columns

## License

GPL-3 (same as RCPA).
