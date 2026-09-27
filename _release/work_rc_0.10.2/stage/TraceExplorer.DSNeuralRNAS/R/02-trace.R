#' Create a canonical TraceExplorer trace
#'
#' @param data data.frame containing at least iter and loss.
#' @param provenance Named list with source information.
#' @param parameter_map Optional te_parameter_map.
#' @return Object of class te_trace.
#' @export
te_trace <- function(data, provenance = list(), parameter_map = NULL) {
  stopifnot(is.data.frame(data))
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  te_validate_trace(data, error = TRUE)

  data$iter <- as.numeric(data$iter)
  data$loss <- as.numeric(data$loss)

  if (!"delta_loss" %in% names(data)) {
    data$delta_loss <- c(NA_real_, diff(data$loss))
  }

  if (!"param_norm" %in% names(data) ||
      !"param_velocity" %in% names(data)) {
    pcols <- .te_parameter_columns(data)
    if (length(pcols) > 0L) {
      theta <- as.matrix(data[, pcols, drop = FALSE])
      storage.mode(theta) <- "double"

      if (!"param_norm" %in% names(data)) {
        data$param_norm <- .te_norm_rows(theta)
      }
      if (!"param_velocity" %in% names(data)) {
        dtheta <- theta[-1L, , drop = FALSE] -
          theta[-nrow(theta), , drop = FALSE]
        data$param_velocity <- c(
          NA_real_,
          .te_norm_rows(dtheta)
        )
      }
    }
  }

  structure(
    list(
      data = data,
      provenance = provenance,
      parameter_map = parameter_map,
      contract_version = "0.1.0"
    ),
    class = "te_trace"
  )
}

#' Extract trace data
#' @param x te_trace.
#' @return data.frame.
#' @export
te_trace_data <- function(x) {
  if (!inherits(x, "te_trace")) {
    stop("Expected object of class te_trace.", call. = FALSE)
  }
  x$data
}

#' Normalized learning time
#'
#' @param trace te_trace or data.frame.
#' @return Numeric vector in [0, 1].
#' @export
te_normalized_time <- function(trace) {
  d <- if (inherits(trace, "te_trace")) te_trace_data(trace) else trace
  k <- as.numeric(d$iter)
  if (length(k) <= 1L || max(k) == min(k)) {
    return(rep(0, length(k)))
  }
  (k - min(k)) / (max(k) - min(k))
}
