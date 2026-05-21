\name{run_http_entrypoint}
\alias{run_http_entrypoint}
\title{Run the full HTTP entry point}
\usage{
run_http_entrypoint(port = NULL, static_port = NULL, daemons = NULL,
                    allowed_origins = NULL, ...)
}
\arguments{
\item{port}{MCP port (default: env RCPA_PORT or 9004).}

\item{static_port}{Static server port (default: env RCPA_STATIC_PORT or 9005).}

\item{daemons}{Mirai daemon count (default: env RCPA_DAEMONS or 4).}

\item{allowed_origins}{Origins for CORS / Origin header validation.}

\item{...}{Forwarded to \code{mcpserver::serve_http}.}
}
\description{
Starts the static server (if not already running) and serves the MCP
server over Streamable HTTP. Daemons are sized via the
\code{RCPA_DAEMONS} env var (default 4). Blocks until shutdown.
}
