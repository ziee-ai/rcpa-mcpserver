# rcpa.mcpserver

[![R-CMD-check](https://github.com/ziee-ai/rcpa-mcpserver/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/ziee-ai/rcpa-mcpserver/actions/workflows/R-CMD-check.yml)

An [MCP](https://modelcontextprotocol.io) server exposing the
[RCPA](https://github.com/tinnlab/RCPA) bioinformatics toolkit over **Streamable
HTTP** or **stdio**. Built on the [`mcpserver`](https://github.com/ziee-ai/mcpserver-r)
R framework. Each analysis runs in a fresh `Rscript` subprocess for isolation;
results are persisted to disk and served back as `resource_link` URLs.

## Install

```r
install.packages("rcpa.mcpserver",
                 repos = c("https://ziee-ai.github.io/drat", getOption("repos")))
```

This pulls `rcpa.mcpserver` and the [`mcpserver`](https://github.com/ziee-ai/mcpserver-r)
framework from the [ziee-ai drat](https://ziee-ai.github.io/drat/); the remaining
dependencies come from CRAN. The analysis tools additionally need `RCPA` plus the
Bioconductor stack (`SummarizedExperiment`, `limma`, `DESeq2`, `edgeR`), which you
install separately (e.g. via `BiocManager`).

## Tools

| Tool | Purpose |
|---|---|
| `validate_input_file` | Sanity-check expression / gene-stats / design CSVs before analysis |
| `run_de_analysis` | Differential expression via limma / DESeq2 / edgeR |
| `run_pathway_analysis` | Pathway / gene-set enrichment (KEGG, GO; 7 methods) |
| `plot_results` | Volcano, MA, bar, heatmap, forest, venn, network, KEGG-map plots |
| `run_consensus_analysis` | Combine 2+ PA methods on one dataset |
| `run_meta_analysis` | Combine DE or PA results across 2+ studies |

The analysis tools are **bidirectional**: an elicitation-capable client is
prompted for missing parameters when they are omitted. URLs (`*_uri`) are
never elicited — pass them exactly as your platform provides them.

## Input formats

* **Expression matrix** — CSV, *genes × samples*: column 1 is the gene ID, the
  header row is sample names (≥ 2), the rest are numeric. Use integer counts for
  DESeq2 / edgeR and normalized values for limma.
* **Gene-stats** — CSV with `id`, `logFC`, and `pvalue` columns (one row per
  gene, unique `id`s). Used as the ranked/scored input to pathway analysis.
* **Experimental design** — CSV with `sample` and `group` columns (one row per
  sample). Add a `pair` column for paired designs, or a `contrast` for designs
  with more than two groups.

---

## Starting the server

Pick whichever fits your deployment. All four launch the **same** server; they
differ only in transport (stdio vs. Streamable HTTP) and how it is hosted.

### 1. On ziee

**a. From the MCP server hub (recommended).** Install it in one click from
ziee's curated hub:

```
{{ZIEE_URL}}/hub/mcp-servers
```

**b. Manually, as a stdio server.** Go to:

```
{{ZIEE_URL}}/settings/mcp-servers
```

In the **Add MCP Server** panel choose the **stdio** transport type, and in the
**command** field put:

```
R -e "install.packages('rcpa.mcpserver', repos=c('https://ziee-ai.github.io/drat', getOption('repos'))); rcpa.mcpserver::start_stdio_server()"
```

`rcpa.mcpserver::start_stdio_server()` speaks the MCP **stdio** transport
(newline-delimited JSON-RPC on stdin/stdout). The one-liner above installs the
package on first launch, then starts it; once installed you can drop the
`install.packages(...)` half and use just
`R -e "rcpa.mcpserver::start_stdio_server()"`. By default results are returned as
local `file://` paths and no HTTP port is opened; set `RCPA_RESULTS_MODE=http` to
start the static results server and emit `http://` links instead. `stdout` is
reserved for the protocol — diagnostics go to `stderr` (or `RCPA_LOG`).

### 2. Over HTTP with Docker

Serves the Streamable HTTP transport plus the static results server:

```bash
docker compose up --build
```

`/mcp` is served on `:9004` and result files on `:9005`. See
[Advanced setup](#advanced-setup) to enable authentication.

### 3. From R (the R interface)

Install once (see [Install](#install)), then start either transport directly:

```r
# Streamable HTTP — /mcp on :9004, static results on :9005
rcpa.mcpserver::run_http_entrypoint()

# stdio — newline-delimited JSON-RPC on stdin/stdout
rcpa.mcpserver::start_stdio_server()
```

Both accept arguments for ports, daemon count, and results mode — see
[Advanced setup](#advanced-setup).

### 4. With conda

A reproducible environment for the package and the `mcpserver` framework is
defined in `environment.yml` (creates the `rcpa-mcp` env):

```bash
conda env create -f environment.yml
# install the mcpserver framework from the ziee-ai drat
conda run -n rcpa-mcp Rscript -e 'install.packages("mcpserver", repos=c("https://ziee-ai.github.io/drat", getOption("repos")))'
conda run -n rcpa-mcp R CMD INSTALL .

# HTTP transport
conda run -n rcpa-mcp Rscript inst/run-http.R
# stdio transport
conda run -n rcpa-mcp Rscript inst/run-stdio.R
```

The conda env intentionally excludes the heavy RCPA + Bioconductor scientific
stack — install it inside the env via `BiocManager` when you need to exercise the
analysis tools; the protocol tests and `validate_input_file` run without it.

---

## Advanced setup

### Configuration parameters

Every knob is an environment variable; the HTTP and stdio entry points read the
same set. **All of these are optional** — the server starts with the defaults
shown, so you only set a variable to override it.

| Env var | Required? | Default | Purpose |
|---|---|---|---|
| `RCPA_PORT` | optional | `9004` | MCP `/mcp` listen port (HTTP transport) |
| `RCPA_HOST` | optional | `0.0.0.0` | MCP bind host |
| `RCPA_STATIC_PORT` | optional | `9005` | Static results server port |
| `RCPA_STATIC_HOST` | optional | `127.0.0.1` | Static server bind host |
| `RCPA_DAEMONS` | optional | `4` | Mirai worker count |
| `RCPA_RESULTS_DIR` | optional | `tempdir()/rcpa-results` | Where job output lands (auto-created on start) |
| `RCPA_RESULTS_MODE` | optional | `file` | stdio only: `file` → `file://` URIs (no HTTP server); `http` → spawn the static server and emit `http://` URIs |
| `BASE_URL` | optional | `http://localhost:9005` | URL prefix for `resource_link`s |
| `RCPA_LOG` | optional | unset → stderr | When set, redirects stderr/log to that file |

The R entry points also take these as named arguments, which override the env
vars:

```r
rcpa.mcpserver::run_http_entrypoint(port = 9004, static_port = 9005, daemons = 6)
rcpa.mcpserver::start_stdio_server(results = "http", daemons = 6)
```

### Authentication

Authentication is **off by default** — deployments stay unauthenticated unless
you opt in. Setting `RCPA_AUTH=on` requires a JWT on every `/mcp` request and
turns on the bundled admin REST API + admin web interface (a user-management
page served in your browser at `/admin/ui`, on the same port as `/mcp`).

> **Auth applies to the HTTP `/mcp` transport only — stdio needs no token.**
> The stdio transport (`rcpa.mcpserver::start_stdio_server()`, used by the ziee
> integration and local clients) is a subprocess your client spawns directly
> over stdin/stdout; it performs no JWT auth and ignores `RCPA_AUTH`. Secure
> it with normal OS process/file permissions instead. The token flow below is
> for the HTTP transport.

Every variable below is **optional**: with `RCPA_AUTH=off` (the default) none
of them are read; once auth is on, each still has a default or is auto-created,
so the only one you should normally set yourself is `MCPSERVER_ADMIN_TOKEN`.

| Env var | Required? | Default / behavior | Purpose |
|---|---|---|---|
| `RCPA_AUTH` | optional | `off` | Master switch. Set `on` to enable JWT auth + admin API + admin web interface. |
| `MCPSERVER_ADMIN_TOKEN` | **set in production** | auto-generated if unset (logged once to stderr / `RCPA_LOG`) | Opaque bootstrap admin token. An auto-generated value does NOT survive a restart, so set it explicitly for any persistent deployment. |
| `RCPA_AUTH_DB` | optional | `<RCPA_RESULTS_DIR>/auth.db` (auto-created) | SQLite store for users + tokens. Mount a persistent volume here in production. |
| `RCPA_AUTH_ISSUER` | optional | `http://127.0.0.1:<RCPA_PORT>` (derived) | JWT `iss` claim. |
| `RCPA_AUTH_AUDIENCE` | optional | `rcpa` | JWT `aud` claim. |
| `RCPA_AUTH_UI` | optional | `on` | Set `off` to hide the bundled `/admin/ui` web interface (REST API stays up). |

Enabling auth requires the `DBI` and `RSQLite` R packages
(`install.packages(c("DBI", "RSQLite"))`).

#### Turn on auth with Docker

The repo ships an overlay that enables auth and mounts a named volume so the
SQLite store survives restarts:

```bash
export MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32)
docker compose -f docker-compose.yaml -f docker-compose.auth.yaml up -d --build
```

The overlay **requires** `MCPSERVER_ADMIN_TOKEN` to be set and fails fast with a
clear error if it is not — so production never launches with an ephemeral token.

#### Turn on auth from R / conda

```bash
export RCPA_AUTH=on
export MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32)
export RCPA_AUTH_DB=/var/lib/rcpa/auth.db   # persistent path
conda run -n rcpa-mcp Rscript inst/run-http.R
```

### Bootstrap the first admin (first run)

The **bootstrap admin token** is the root credential — it is how you get your
first admin account without there being a user in the database yet.

**Create a bootstrap admin token.** It is just a long, high-entropy opaque
string — generate one however you like and keep it secret:

```bash
openssl rand -hex 32                              # 64 hex chars (recommended)
# alternatives:
python3 -c "import secrets; print(secrets.token_hex(32))"
head -c 32 /dev/urandom | base64                  # any high-entropy string works
```

Then:

1. **Set it before the first start** and export it as `MCPSERVER_ADMIN_TOKEN`
   (`export MCPSERVER_ADMIN_TOKEN=<the value>`), or pass it via your compose
   `.env` / secret manager. If you skip this, the server auto-generates one and
   logs it **once** to stderr / `RCPA_LOG` — fine for a quick local test, but
   it is lost on restart, so set it explicitly for anything persistent.
2. **Start the server with `RCPA_AUTH=on`** (see above). On first start it
   auto-creates the SQLite store at `RCPA_AUTH_DB`.
3. The bootstrap token now authenticates against the admin REST API and the
   admin UI as a full admin. Use it to create real user accounts and mint their
   tokens (below). Treat it like a root password — rotate it by changing
   `MCPSERVER_ADMIN_TOKEN` and restarting.

### Access the user-management UI

With auth on, open the bundled admin web interface in a browser:

```
http://localhost:9004/admin/ui
```

Log in with your `MCPSERVER_ADMIN_TOKEN`. The UI lists users and lets you
create, edit, and delete them, and mint or revoke their tokens. (To disable the
web interface, set `RCPA_AUTH_UI=off`; the REST API under `/admin/*` stays
available.)

### Create a user and mint a token

**Via the UI:** create a user, open their **Tokens** tab, and click mint. The
JWT is shown **once** in a modal — copy it immediately. Clients then send it as
`Authorization: Bearer <jwt>` on every `/mcp` request.

**Via the REST API** (same `/admin/*` surface the UI uses; authenticate with the
bootstrap token or an admin user's JWT):

```bash
ADMIN=$MCPSERVER_ADMIN_TOKEN
BASE=http://localhost:9004

# 1. create a user
curl -s -X POST $BASE/admin/users \
  -H "Authorization: Bearer $ADMIN" -H 'Content-Type: application/json' \
  -d '{"username":"alice","is_admin":false}'
# -> {"id":"<user_id>", ...}

# 2. mint a token for that user (ttl in seconds; capped at 1 year)
curl -s -X POST $BASE/admin/tokens/mint \
  -H "Authorization: Bearer $ADMIN" -H 'Content-Type: application/json' \
  -d '{"user_id":"<user_id>","name":"laptop","ttl":2592000}'
# -> {"jti":"...","token":"<jwt>","expires_at":...}   (the JWT is the "token" field, returned once)

# 3. the client now calls /mcp with that JWT
curl -s $BASE/mcp -H "Authorization: Bearer <jwt>" ...
```

Admin REST routes: `GET /admin/healthz`, `GET|POST /admin/users`,
`GET|PATCH|DELETE /admin/users/{id}`, `GET /admin/users/{id}/tokens`,
`POST /admin/tokens/mint`, `POST /admin/tokens/{jti}/revoke`,
`POST /admin/tokens/{jti}/reactivate`, `DELETE /admin/tokens/{jti}`.

> **Note:** the static results server (port `9005`) is **not** behind
> the JWT. Its `resource_link` URLs are unguessable but not access-controlled —
> put a reverse proxy in front of it if you need per-user control over outputs.

### Full-parameter launch examples

The three examples below start the **HTTP transport** with *every* knob set
explicitly — the configuration variables plus authentication. They are
equivalent: the same env-var contract, just delivered differently. Drop the
`RCPA_AUTH*` / `MCPSERVER_ADMIN_TOKEN` lines to run unauthenticated.

> **How the env vars are read.** Most variables are read when the server
> starts. Two of them — `RCPA_RESULTS_DIR` and `BASE_URL` — are read **once,
> when the package is loaded** (`.onLoad`), so they must already be set *before*
> the package loads. With Docker and conda this is automatic (the variables are
> in the environment before the R process starts); in an interactive R session
> set them with `Sys.setenv()` **before** `library(rcpa.mcpserver)` (shown below).

#### Docker — full parameters

```bash
docker build -t rcpa.mcpserver:latest .

export MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32)
echo "admin token (needed to log into /admin/ui): $MCPSERVER_ADMIN_TOKEN"

docker run -d --name rcpa.mcpserver \
  -p 9004:9004 -p 9005:9005 \
  -v rcpa-results:/var/lib/rcpa/results \
  -v rcpa-auth:/var/lib/rcpa/auth \
  `# --- configuration ---` \
  -e RCPA_PORT=9004 \
  -e RCPA_HOST=0.0.0.0 \
  -e RCPA_STATIC_PORT=9005 \
  -e RCPA_STATIC_HOST=0.0.0.0 \
  -e RCPA_DAEMONS=6 \
  -e RCPA_RESULTS_DIR=/var/lib/rcpa/results \
  -e BASE_URL=http://localhost:9005 \
  -e RCPA_LOG=/var/log/rcpa.log \
  `# --- authentication ---` \
  -e RCPA_AUTH=on \
  -e MCPSERVER_ADMIN_TOKEN="$MCPSERVER_ADMIN_TOKEN" \
  -e RCPA_AUTH_DB=/var/lib/rcpa/auth/auth.db \
  -e RCPA_AUTH_ISSUER=https://rcpa.example.com \
  -e RCPA_AUTH_AUDIENCE=rcpa \
  -e RCPA_AUTH_UI=on \
  rcpa.mcpserver:latest
```

`RCPA_STATIC_HOST=0.0.0.0` is required so the static results port is
reachable from outside the container. Set `BASE_URL` (and
`RCPA_AUTH_ISSUER`) to the host's externally reachable URL in a real
deployment, not `localhost`.

#### R interface — full parameters

```r
# RESULTS_DIR and BASE_URL must be set BEFORE the package loads (.onLoad reads
# them once), so call Sys.setenv() before library().
Sys.setenv(
  # --- configuration ---
  RCPA_PORT        = "9004",
  RCPA_HOST        = "0.0.0.0",
  RCPA_STATIC_PORT = "9005",
  RCPA_STATIC_HOST = "0.0.0.0",
  RCPA_DAEMONS     = "6",
  RCPA_RESULTS_DIR = "/var/lib/rcpa/results",
  BASE_URL         = "http://localhost:9005",
  RCPA_LOG         = "/var/log/rcpa.log",
  # --- authentication ---
  RCPA_AUTH             = "on",
  MCPSERVER_ADMIN_TOKEN = "replace-with-openssl-rand-hex-32",
  RCPA_AUTH_DB          = "/var/lib/rcpa/auth/auth.db",
  RCPA_AUTH_ISSUER      = "https://rcpa.example.com",
  RCPA_AUTH_AUDIENCE    = "rcpa",
  RCPA_AUTH_UI          = "on"
)

library(rcpa.mcpserver)

# Ports / static port / daemon count can come from the env vars above, or be
# passed explicitly as arguments (arguments win over the env vars):
run_http_entrypoint(port = 9004, static_port = 9005, daemons = 6)
```

#### conda — full parameters

```bash
export MCPSERVER_ADMIN_TOKEN=$(openssl rand -hex 32)
echo "admin token (needed to log into /admin/ui): $MCPSERVER_ADMIN_TOKEN"

# --- configuration ---
export RCPA_PORT=9004
export RCPA_HOST=0.0.0.0
export RCPA_STATIC_PORT=9005
export RCPA_STATIC_HOST=0.0.0.0
export RCPA_DAEMONS=6
export RCPA_RESULTS_DIR=/var/lib/rcpa/results
export BASE_URL=http://localhost:9005
export RCPA_LOG=/var/log/rcpa.log
# --- authentication ---
export RCPA_AUTH=on
export RCPA_AUTH_DB=/var/lib/rcpa/auth/auth.db
export RCPA_AUTH_ISSUER=https://rcpa.example.com
export RCPA_AUTH_AUDIENCE=rcpa
export RCPA_AUTH_UI=on

# the exported environment is inherited by the R process, so .onLoad sees it
conda run -n rcpa-mcp Rscript inst/run-http.R
```

For the **stdio** transport, the relevant knobs are fewer —
`RCPA_RESULTS_MODE` (`file` default, or `http`), `RCPA_DAEMONS`,
`RCPA_RESULTS_DIR`, `RCPA_LOG` (plus `RCPA_STATIC_PORT` /
`RCPA_STATIC_HOST` / `BASE_URL` only when `RCPA_RESULTS_MODE=http`).
Export them the same way, then run `Rscript inst/run-stdio.R` (or
`rcpa.mcpserver::start_stdio_server()`). Authentication does not apply to stdio.

---

## Tests

```bash
conda run -n rcpa-mcp Rscript -e 'testthat::test_local(".")'   # unit + dispatch + integration
conda run -n rcpa-mcp R CMD check --as-cran .
```

Tier-3 template tests exercise real RCPA via subprocess and are skipped unless
`RCPA_RUN_TEMPLATE_TESTS=1` is set; add `RCPA_RUN_PA_TESTS=1` for the
KEGG-network pathway / consensus / meta-PA tests (requires internet):

```bash
RCPA_RUN_TEMPLATE_TESTS=1 conda run -n rcpa-mcp Rscript -e 'testthat::test_local(".")'
RCPA_RUN_TEMPLATE_TESTS=1 RCPA_RUN_PA_TESTS=1 \
  conda run -n rcpa-mcp Rscript -e 'testthat::test_local(".")'
```

## Attribution / License

Licensed under **GPL-3** (same as RCPA). RCPA toolkit © the
[tinnlab](https://github.com/tinnlab/RCPA) authors.
