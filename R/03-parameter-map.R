#' Create a long-form parameter map
#'
#' A parameter map retains architecture-specific detail without forcing
#' the canonical trace to contain thousands or millions of columns.
#'
#' @param data data.frame with iter, layer, parameter_type, parameter_id, value.
#' @return te_parameter_map.
#' @export
te_parameter_map <- function(data) {
  req <- c("iter", "layer", "parameter_type", "parameter_id", "value")
  if (!is.data.frame(data) || !all(req %in% names(data))) {
    stop(
      "Parameter map requires: ",
      paste(req, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  out <- data[, req, drop = FALSE]
  out$iter <- as.numeric(out$iter)
  out$value <- as.numeric(out$value)
  structure(
    list(data = out, contract_version = "0.1.0"),
    class = "te_parameter_map"
  )
}

#' Extract parameter-map data
#' @param x te_parameter_map.
#' @export
te_parameter_map_data <- function(x) {
  if (!inherits(x, "te_parameter_map")) {
    stop("Expected te_parameter_map.", call. = FALSE)
  }
  x$data
}
