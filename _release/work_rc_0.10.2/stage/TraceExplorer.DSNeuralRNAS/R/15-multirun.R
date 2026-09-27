#' Create a multi-run TraceExplorer collection
#'
#' A multi-run object preserves each trace independently. No interpolation,
#' pooling, averaging of states, or semantic promotion is performed.
#'
#' @param traces List of te_trace objects.
#' @param run_ids Optional unique run identifiers.
#' @param labels Optional human-readable labels.
#' @return te_multirun.
#' @export
te_multirun <- function(traces, run_ids = NULL, labels = NULL) {
  if (!is.list(traces) || length(traces) < 1L) {
    stop("traces must be a non-empty list.", call. = FALSE)
  }

  ok <- vapply(
    traces,
    inherits,
    logical(1),
    what = "te_trace"
  )
  if (!all(ok)) {
    stop(
      "Every element of traces must inherit te_trace.",
      call. = FALSE
    )
  }

  n <- length(traces)

  if (is.null(run_ids)) {
    run_ids <- sprintf("run_%03d", seq_len(n))
  }
  run_ids <- as.character(run_ids)

  if (length(run_ids) != n || any(!nzchar(run_ids))) {
    stop(
      "run_ids must contain one non-empty identifier per trace.",
      call. = FALSE
    )
  }
  if (anyDuplicated(run_ids)) {
    stop("run_ids must be unique.", call. = FALSE)
  }

  if (is.null(labels)) {
    labels <- run_ids
  }
  labels <- as.character(labels)

  if (length(labels) != n) {
    stop(
      "labels must contain one value per trace.",
      call. = FALSE
    )
  }

  names(traces) <- run_ids

  out <- structure(
    list(
      traces = traces,
      run_ids = run_ids,
      labels = labels,
      contract_version = "0.5.0"
    ),
    class = "te_multirun"
  )

  te_validate_multirun(out, error = TRUE)
  out
}

#' Validate a multi-run collection
#'
#' @param x te_multirun.
#' @param error Stop on failure.
#' @return Validation table.
#' @export
te_validate_multirun <- function(x, error = FALSE) {
  checks <- data.frame(
    check = character(),
    status = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )

  add <- function(check, ok, detail) {
    checks <<- rbind(
      checks,
      data.frame(
        check = check,
        status = if (isTRUE(ok)) "PASS" else "FAIL",
        detail = detail,
        stringsAsFactors = FALSE
      )
    )
  }

  is_multi <- inherits(x, "te_multirun")
  add(
    "class",
    is_multi,
    "Object must inherit te_multirun."
  )

  if (is_multi) {
    n <- length(x$traces)

    add(
      "non_empty",
      n >= 1L,
      "At least one trace is required."
    )
    add(
      "all_traces",
      all(vapply(
        x$traces,
        inherits,
        logical(1),
        what = "te_trace"
      )),
      "Every run must preserve an independent te_trace."
    )
    add(
      "run_ids",
      length(x$run_ids) == n &&
        !anyDuplicated(x$run_ids) &&
        all(nzchar(x$run_ids)),
      "Run identifiers must be complete and unique."
    )
    add(
      "labels",
      length(x$labels) == n,
      "Each run must have one display label."
    )

    trace_valid <- vapply(
      x$traces,
      function(tr) {
        val <- te_validate_trace(
          te_trace_data(tr),
          error = FALSE
        )
        all(val$status == "PASS")
      },
      logical(1)
    )
    add(
      "trace_contracts",
      all(trace_valid),
      "All runs must satisfy the canonical trace contract."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Multi-run validation failed:",
        paste(
          checks$detail[checks$status == "FAIL"],
          collapse = "\n"
        ),
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  checks
}

.te_optional_numeric <- function(d, name) {
  if (name %in% names(d)) {
    as.numeric(d[[name]])
  } else {
    rep(NA_real_, nrow(d))
  }
}

.te_prov_value <- function(trace, name, fallback = NA_character_) {
  p <- trace$provenance
  x <- p[[name]]
  if (is.null(x) || !length(x) || all(is.na(x))) {
    return(fallback)
  }
  as.character(x[[1L]])
}

#' Long canonical data for multiple runs
#'
#' Normalized time tau is added for visual alignment only. Original iterations
#' are retained and no interpolation is performed.
#'
#' @param x te_multirun.
#' @return data.frame.
#' @export
te_multirun_trace_data <- function(x) {
  te_validate_multirun(x, error = TRUE)

  rows <- vector("list", length(x$traces))

  for (i in seq_along(x$traces)) {
    tr <- x$traces[[i]]
    d <- te_trace_data(tr)

    rows[[i]] <- data.frame(
      run_id = x$run_ids[[i]],
      run_label = x$labels[[i]],
      iter = as.numeric(d$iter),
      tau = te_normalized_time(tr),
      loss = as.numeric(d$loss),
      delta_loss = .te_optional_numeric(d, "delta_loss"),
      grad_norm = .te_optional_numeric(d, "grad_norm"),
      eta = .te_optional_numeric(d, "eta"),
      param_norm = .te_optional_numeric(d, "param_norm"),
      param_velocity =
        .te_optional_numeric(d, "param_velocity"),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Descriptive index of a multi-run collection
#'
#' This table is descriptive only. It does not rank runs or choose a winner.
#'
#' @param x te_multirun.
#' @return data.frame.
#' @export
te_multirun_index <- function(x) {
  te_validate_multirun(x, error = TRUE)

  rows <- vector("list", length(x$traces))

  for (i in seq_along(x$traces)) {
    tr <- x$traces[[i]]
    d <- te_trace_data(tr)
    cap <- te_capabilities(tr)

    loss_initial <- d$loss[[1L]]
    loss_final <- d$loss[[nrow(d)]]

    relative_reduction <- if (
      is.finite(loss_initial) &&
      abs(loss_initial) > .Machine$double.eps
    ) {
      (loss_initial - loss_final) / abs(loss_initial)
    } else {
      NA_real_
    }

    arch_layers <- if (
      inherits(tr$architecture, "te_architecture")
    ) {
      tr$architecture$n_layers
    } else {
      NA_integer_
    }

    arch_parameters <- if (
      inherits(tr$architecture, "te_architecture")
    ) {
      tr$architecture$n_parameters
    } else {
      NA_real_
    }

    rows[[i]] <- data.frame(
      run_id = x$run_ids[[i]],
      run_label = x$labels[[i]],
      source_package =
        .te_prov_value(tr, "source_package"),
      source_class =
        .te_prov_value(tr, "source_class"),
      model_id =
        .te_prov_value(tr, "model_id"),
      seed =
        suppressWarnings(as.integer(
          .te_prov_value(tr, "seed", NA_character_)
        )),
      n_states = nrow(d),
      iter_min = min(d$iter),
      iter_max = max(d$iter),
      loss_initial = loss_initial,
      loss_final = loss_final,
      loss_change = loss_final - loss_initial,
      relative_loss_reduction = relative_reduction,
      capability = cap$capability[[1L]],
      architecture_layers = arch_layers,
      architecture_parameters = arch_parameters,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Architecture comparison table across runs
#'
#' Missing architecture contracts remain explicit NA values.
#'
#' @param x te_multirun.
#' @return data.frame.
#' @export
te_multirun_architectures <- function(x) {
  te_validate_multirun(x, error = TRUE)

  rows <- lapply(seq_along(x$traces), function(i) {
    tr <- x$traces[[i]]

    if (!inherits(tr$architecture, "te_architecture")) {
      return(
        data.frame(
          run_id = x$run_ids[[i]],
          run_label = x$labels[[i]],
          model_id = .te_prov_value(tr, "model_id"),
          architecture_available = FALSE,
          n_layers = NA_integer_,
          n_parameters = NA_real_,
          layer_signature = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }

    a <- te_architecture_data(tr$architecture)
    signature <- paste(
      c(
        a$input_dim[[1L]],
        a$output_dim
      ),
      collapse = "\u2192"
    )

    data.frame(
      run_id = x$run_ids[[i]],
      run_label = x$labels[[i]],
      model_id = tr$architecture$model_id,
      architecture_available = TRUE,
      n_layers = tr$architecture$n_layers,
      n_parameters = tr$architecture$n_parameters,
      layer_signature = signature,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Build a real ML.DSNeuralRNAS multi-run example
#'
#' The runs share the same deterministic dataset and differ only through the
#' declared seeds and hidden-layer widths.
#'
#' @param iter Training iterations per run.
#' @param seeds Integer seeds.
#' @param d_hidden Hidden widths. Scalar values are recycled.
#' @param eta Learning rate. Scalar values are recycled.
#' @return te_multirun.
#' @export
te_example_multirun_ml <- function(
    iter = 60L,
    seeds = c(101L, 202L, 303L),
    d_hidden = c(4L, 5L, 7L),
    eta = 0.04) {

  n <- max(length(seeds), length(d_hidden), length(eta))

  seeds <- rep(seeds, length.out = n)
  d_hidden <- rep(d_hidden, length.out = n)
  eta <- rep(eta, length.out = n)

  traces <- vector("list", n)
  ids <- character(n)
  labels <- character(n)

  for (i in seq_len(n)) {
    src <- te_example_ml_variable(
      iter = iter,
      seed = seeds[[i]],
      d_hidden = d_hidden[[i]],
      eta = eta[[i]]
    )

    traces[[i]] <- te_bridge_ml_variable(src)
    ids[[i]] <- sprintf("MLR%02d", i)
    labels[[i]] <- paste0(
      "ML.DSNeuralRNAS \u00b7 h=",
      d_hidden[[i]],
      " \u00b7 seed=",
      seeds[[i]]
    )

    traces[[i]]$provenance$experiment_id <- ids[[i]]
  }

  te_multirun(
    traces = traces,
    run_ids = ids,
    labels = labels
  )
}
