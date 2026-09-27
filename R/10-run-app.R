#' Launch TraceExplorer Shiny application
#'
#' @export
run_trace_explorer <- function() {
  .te_require_namespace("shiny", "TraceExplorer Shiny application")
  app_dir <- system.file("shiny", "app", package = "TraceExplorer.DSNeuralRNAS")
  if (!nzchar(app_dir)) {
    stop("Installed Shiny app directory was not found.", call. = FALSE)
  }
  shiny::runApp(app_dir)
}
