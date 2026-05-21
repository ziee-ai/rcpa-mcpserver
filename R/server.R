#' Build the RCPA MCP server
#'
#' Constructs an mcpserver McpServer with all 6 RCPA analysis tools
#' registered. The returned object can be passed to mcpserver::serve_http
#' or used directly with mcpserver::route_message for testing.
#'
#' @return An McpServer object.
#' @export
build_rcpa_server <- function() {
  srv <- mcpserver::new_server(
    name = "rcpa-mcpserver",
    title = "RCPA Bioinformatics MCP Server",
    version = utils::packageVersion("rcpa.mcpserver"),
    instructions = paste(
      "This server exposes the RCPA bioinformatics toolkit",
      "(differential expression, pathway analysis, consensus,",
      "meta-analysis, plotting).",
      "Always call validate_input_file before running an analysis tool",
      "to confirm the input format is correct.",
      "All *_uri parameters must be URLs from your platform - do not",
      "construct or modify them."),
    description = "RCPA workflows as MCP tools.",
    website_url = "https://github.com/tinnlab/RCPA"
  )
  mcpserver::add_capability(srv, tool_validate_input_file())
  mcpserver::add_capability(srv, tool_run_de_analysis())
  mcpserver::add_capability(srv, tool_run_pathway_analysis())
  mcpserver::add_capability(srv, tool_plot_results())
  mcpserver::add_capability(srv, tool_run_consensus_analysis())
  mcpserver::add_capability(srv, tool_run_meta_analysis())
  srv
}
