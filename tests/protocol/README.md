# Manual protocol smoke tests

These bash scripts exercise the live server over real HTTP. They are
**not** part of `R CMD check` — run them manually before tagging a
release.

Prerequisites: a running server reachable at `http://localhost:9004`
(launch via `R -e 'rcpa.mcpserver::run_http_entrypoint()'` or
`docker compose up`).

Each script initializes a session, calls `tools/list`, invokes a tool
end-to-end, and prints the response.
