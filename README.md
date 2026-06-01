# rcpa.mcpserver

MCP server exposing the [RCPA](https://github.com/tinnlab/RCPA)
bioinformatics toolkit over **Streamable HTTP** or **stdio**. Built on
the [mcpserver](../mcpserver-r/) R framework.

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

## Conda environment

A reproducible env for both `rcpa.mcpserver` and the `mcpserver` R
framework lives in `environment.yml`:

```sh
conda env create -f environment.yml
# mcpserver-r is not on CRAN yet; install from the sibling source tree:
conda run -n rcpa-mcp R CMD INSTALL /data/khoi/home-iso/mcp-hubs/mcpserver-r
conda run -n rcpa-mcp R CMD INSTALL .

# HTTP transport
conda run -n rcpa-mcp Rscript inst/run-http.R
# stdio transport (for Claude Desktop and similar MCP clients)
conda run -n rcpa-mcp Rscript inst/run-stdio.R
```

The conda env intentionally excludes the heavy RCPA + Bioconductor
scientific stack (`RCPA`, `SummarizedExperiment`, `limma`, `DESeq2`,
`edgeR`, `ROntoTools`, `fgsea`, ...) — some aren't on conda-forge.
Install them inside the env via `BiocManager` when you need to exercise
the analysis tools; the stdio protocol tests and `validate_input_file`
run without them.

## stdio transport

`inst/run-stdio.R` launches the server over newline-delimited JSON-RPC
on stdin/stdout. `stdout` is reserved for the protocol; diagnostics go
to `stderr` (or `RCPA_LOG`). By default tool results are returned as
`file://` URIs into `RCPA_RESULTS_DIR`, so no HTTP listener is needed.

| Env var | Default | Purpose |
|---|---|---|
| `RCPA_RESULTS_MODE` | `file` | `file` → `file://` URIs (no HTTP server). `http` → spawns the static server and emits `http://` URIs. |
| `RCPA_DAEMONS` | `4` | Mirai worker count |
| `RCPA_RESULTS_DIR` | `tempdir()/rcpa-results` | Where job output lands |
| `RCPA_LOG` | unset | When set, redirects stderr to that file |
| `RCPA_STATIC_PORT` / `RCPA_STATIC_HOST` / `BASE_URL` | — | Only used when `RCPA_RESULTS_MODE=http` |

MCP client config (Claude Desktop format):

```json
{
  "mcpServers": {
    "rcpa": {
      "command": "conda",
      "args": ["run", "-n", "rcpa-mcp", "Rscript",
               "/path/to/rcpa-mcpserver/inst/run-stdio.R"],
      "env": {
        "RCPA_RESULTS_MODE": "file",
        "RCPA_LOG": "/tmp/rcpa-mcp.log"
      }
    }
  }
}
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

## Authentication

Authentication is **off by default** — existing deployments keep working
unchanged. Set `RCPA_AUTH=on` to require a JWT on every `/mcp` request
and gain the bundled admin REST API + admin SPA (served at
`/admin/ui` on the same port as `/mcp`).

| Env var | Default | Purpose |
|---|---|---|
| `RCPA_AUTH` | `off` | Master switch. Set `on` to enable. |
| `MCPSERVER_ADMIN_TOKEN` | auto-generated | Opaque bootstrap admin token. **Set explicitly in production** — auto-generated values do NOT survive container restart. |
| `RCPA_AUTH_DB` | `${RCPA_RESULTS_DIR}/auth.db` | SQLite path for the users + tokens store. Mount a persistent volume here in production. |
| `RCPA_AUTH_ISSUER` | `http://127.0.0.1:${RCPA_PORT}` | JWT `iss` claim. |
| `RCPA_AUTH_AUDIENCE` | `rcpa` | JWT `aud` claim. |
| `RCPA_AUTH_UI` | `on` | Set `off` to hide the bundled `/admin/ui` SPA. |

### Production deployment with auth

Use the included overlay:

```sh
export MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32)
docker compose -f docker-compose.yaml -f docker-compose.auth.yaml up -d --build

# Browser: http://localhost:9004/admin/ui  → log in with $MCPSERVER_ADMIN_TOKEN
# Mint a token under the user's "Tokens" tab; clients then send it as
#   Authorization: Bearer <jwt>
# on every /mcp request.
```

The overlay enables auth, requires `MCPSERVER_ADMIN_TOKEN` to be set
(`docker compose` fails with a clear error otherwise), and mounts a
named volume so the SQLite store survives container restarts.

### Known gaps

- The static results server (port 9005) is **not** behind the JWT.
  The `resource_link` URLs it serves are unguessable but not
  access-controlled. Put a reverse proxy in front of it if you need
  per-user access control on analysis outputs.
- Subprocess tools (`Rscript --vanilla`) don't see the auth context —
  they receive their arguments directly from MCP, not via re-auth.
- The RS256 signing key is auto-generated in memory on first start;
  restart invalidates every minted token. An explicit
  `RCPA_AUTH_SIGNING_KEY=/path/to/key.pem` knob is a planned follow-up.

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

The auth integration tests run in both modes — once with the default
unauthenticated path and once with full JWT enforcement. Both must be
green before any release:

```sh
NOT_CRAN=true Rscript -e 'testthat::test_local(".")'           # all modes
bash tests/protocol/docker-smoke.sh         # container with RCPA_AUTH off (default)
MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32) \
  bash tests/protocol/docker-smoke-auth.sh  # container with RCPA_AUTH on
```

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

Tier 5 also includes the two auth integration suites — they spawn the
server via `processx` and exercise both modes:

- `tests/testthat/test-auth-off-integration.R` — `/mcp` reachable without
  bearer, `/admin/*` returns 404, plus a `validate_input_file` pipeline
  smoke.
- `tests/testthat/test-auth-on-integration.R` — full mint → use → revoke
  → 401 cycle, admin REST + SPA shell, non-admin 403, persistence across
  restart, plus a JWT-authorized `validate_input_file` pipeline smoke.

Expected baseline (auth tests included): **378 passed, 0 failed, 14
skipped** (skip count grows when `RCPA_RUN_TEMPLATE_TESTS` and
`RCPA_RUN_PA_TESTS` are unset).

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
