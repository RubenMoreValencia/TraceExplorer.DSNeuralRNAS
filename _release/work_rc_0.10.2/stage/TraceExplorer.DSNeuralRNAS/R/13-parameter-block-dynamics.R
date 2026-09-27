.te_parameter_block_key <- function(layer, parameter_type) {
  paste(layer, parameter_type, sep = "::")
}

#' Summarize parameter history by layer and parameter type
#'
#' @param trace te_trace with exact parameter_map.
#' @return data.frame with block norm and block velocity by iteration.
#' @export
te_parameter_block_dynamics <- function(trace) {
  if (!inherits(trace, "te_trace")) {
    stop("Expected te_trace.", call. = FALSE)
  }
  if (!inherits(trace$parameter_map, "te_parameter_map")) {
    stop(
      "Exact parameter history is required for block dynamics.",
      call. = FALSE
    )
  }

  d <- te_parameter_map_data(trace$parameter_map)
  d$block_id <- .te_parameter_block_key(
    d$layer,
    d$parameter_type
  )

  groups <- split(
    d,
    interaction(
      d$iter,
      d$block_id,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  rows <- lapply(groups, function(g) {
    data.frame(
      iter = g$iter[[1L]],
      layer = g$layer[[1L]],
      parameter_type = g$parameter_type[[1L]],
      block_id = g$block_id[[1L]],
      n_parameters = nrow(g),
      block_norm = sqrt(sum(g$value^2)),
      block_mean = mean(g$value),
      block_sd = if (nrow(g) > 1L) stats::sd(g$value) else 0,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  out <- out[
    order(out$block_id, out$iter),
    ,
    drop = FALSE
  ]

  out$block_velocity <- NA_real_

  for (bid in unique(out$block_id)) {
    idx_out <- which(out$block_id == bid)
    g <- d[d$block_id == bid, , drop = FALSE]
    iters <- sort(unique(g$iter))

    if (length(iters) <= 1L) {
      next
    }

    v <- rep(NA_real_, length(iters))

    for (i in 2:length(iters)) {
      prev <- g[g$iter == iters[[i - 1L]], , drop = FALSE]
      curr <- g[g$iter == iters[[i]], , drop = FALSE]

      prev <- prev[order(prev$parameter_id), , drop = FALSE]
      curr <- curr[order(curr$parameter_id), , drop = FALSE]

      if (!identical(prev$parameter_id, curr$parameter_id)) {
        stop(
          "Parameter identities changed within block ",
          bid,
          ".",
          call. = FALSE
        )
      }

      v[[i]] <- sqrt(sum((curr$value - prev$value)^2))
    }

    map <- match(out$iter[idx_out], iters)
    out$block_velocity[idx_out] <- v[map]
  }

  out
}

#' Aggregate parameter dynamics at layer level
#'
#' @param trace te_trace with exact parameter_map.
#' @return data.frame.
#' @export
te_layer_dynamics <- function(trace) {
  b <- te_parameter_block_dynamics(trace)

  groups <- split(
    b,
    interaction(
      b$iter,
      b$layer,
      drop = TRUE,
      lex.order = TRUE
    )
  )

  rows <- lapply(groups, function(g) {
    data.frame(
      iter = g$iter[[1L]],
      layer = g$layer[[1L]],
      n_parameters = sum(g$n_parameters),
      layer_param_norm = sqrt(sum(g$block_norm^2)),
      layer_velocity = if (all(is.na(g$block_velocity))) {
        NA_real_
      } else {
        sqrt(sum(g$block_velocity^2, na.rm = TRUE))
      },
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$layer, out$iter), , drop = FALSE]
}

#' Verify global norm decomposition from parameter map
#'
#' @param trace te_trace with parameter_map and param_norm.
#' @return One-row data.frame.
#' @export
te_verify_parameter_decomposition <- function(trace) {
  d <- te_trace_data(trace)

  if (!"param_norm" %in% names(d)) {
    stop("Trace does not expose param_norm.", call. = FALSE)
  }

  pm <- te_parameter_map_data(trace$parameter_map)

  calc <- vapply(
    split(pm$value, pm$iter),
    function(v) sqrt(sum(v^2)),
    numeric(1)
  )

  calc_iter <- as.numeric(names(calc))
  m <- match(d$iter, calc_iter)

  err <- abs(
    d$param_norm -
      unname(calc[m])
  )

  data.frame(
    n_iterations = nrow(d),
    max_abs_error = max(err, na.rm = TRUE),
    mean_abs_error = mean(err, na.rm = TRUE),
    exact_within_1e_12 = max(err, na.rm = TRUE) <= 1e-12,
    stringsAsFactors = FALSE
  )
}
