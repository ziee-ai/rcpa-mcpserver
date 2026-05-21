\name{spawn_static_server}
\alias{spawn_static_server}
\title{Spawn a read-only static file HTTP server for the results directory}
\usage{
spawn_static_server(dir = results_dir(), port = NULL, host = NULL)
}
\arguments{
\item{dir}{Directory to serve (defaults to the package's results dir).}

\item{port}{TCP port (defaults to env var RCPA_STATIC_PORT or 9005).}

\item{host}{Bind host (defaults to env var RCPA_STATIC_HOST or "127.0.0.1").}
}
\value{
The nanonext server handle. The caller is responsible for keeping
it alive (or storing it in package state).
}
\description{
Used by the run entry point to serve generated CSVs, RDS, and PNGs
back to clients via the \code{resource_link} URLs the analysis tools
emit. Defense-in-depth: rejects paths containing \code{..} to prevent
traversal, and refuses to serve anything outside the configured
results directory.
}
