\name{build_rcpa_server}
\alias{build_rcpa_server}
\title{Build the RCPA MCP server}
\usage{
build_rcpa_server()
}
\value{
An \code{McpServer} object from the mcpserver package, with all 6
RCPA analysis tools registered: \code{validate_input_file},
\code{run_de_analysis}, \code{run_pathway_analysis},
\code{plot_results}, \code{run_consensus_analysis},
\code{run_meta_analysis}.
}
\description{
Constructs an mcpserver \code{McpServer} with all 6 RCPA analysis
tools registered. The returned object can be passed to
\code{mcpserver::serve_http} or used directly with
\code{mcpserver::route_message} for testing.
}
\examples{
srv <- build_rcpa_server()
ls(srv$tools, all.names = TRUE)
}
