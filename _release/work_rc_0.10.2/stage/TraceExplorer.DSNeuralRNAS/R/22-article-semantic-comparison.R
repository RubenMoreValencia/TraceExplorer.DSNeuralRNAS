.te_case_iter_to_tau <- function(trace, iter) {
  if (!inherits(trace, "te_trace")) {
    return(rep(NA_real_, length(iter)))
  }

  d <- te_trace_data(trace)
  tau <- te_normalized_time(trace)

  m <- match(iter, d$iter)
  tau[m]
}

#' Existing semantic timeline across article cases
#'
#' Only article cases with an existing te_formalspec_analysis are included.
#' Source-only cases are never silently reclassified.
#'
#' @param x te_article_case_set.
#' @return data.frame with Psi, Gamma and exact temporal coordinates.
#' @export
te_article_case_set_semantic_timeline <- function(x) {
  te_validate_article_case_set(x, error = TRUE)

  rows <- list()
  q <- 1L

  for (i in seq_along(x$cases)) {
    case <- x$cases[[i]]

    if (!inherits(
      case$analysis,
      "te_formalspec_analysis"
    )) {
      next
    }

    win <- DSNeuralRNAS.FormalSpec::dsfs_windows_data(
      case$analysis$windows
    )
    cnd <- DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
      case$analysis$candidates
    )
    cnf <- DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
      case$analysis$confirmed
    )

    if (!nrow(win)) {
      next
    }

    req_win <- c(
      "window_id",
      "start_iter",
      "end_iter"
    )
    miss_win <- setdiff(req_win, names(win))
    if (length(miss_win)) {
      stop(
        "FormalSpec windows are missing: ",
        paste(miss_win, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    req_cnd <- c(
      "window_id",
      "candidate_regime"
    )
    miss_cnd <- setdiff(req_cnd, names(cnd))
    if (length(miss_cnd)) {
      stop(
        "FormalSpec candidate regimes are missing: ",
        paste(miss_cnd, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    req_cnf <- c(
      "window_id",
      "confirmed_regime"
    )
    miss_cnf <- setdiff(req_cnf, names(cnf))
    if (length(miss_cnf)) {
      stop(
        "FormalSpec confirmed regimes are missing: ",
        paste(miss_cnf, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    m_cnd <- match(win$window_id, cnd$window_id)
    m_cnf <- match(win$window_id, cnf$window_id)

    if (anyNA(m_cnd) || anyNA(m_cnf)) {
      stop(
        "FormalSpec window_id alignment failed for ",
        x$case_keys[[i]],
        ".",
        call. = FALSE
      )
    }

    tau <- .te_case_iter_to_tau(
      case$trace,
      win$end_iter
    )

    rows[[q]] <- data.frame(
      case_key = x$case_keys[[i]],
      experiment = case$experiment,
      stage = case$stage,
      case_id = case$case_id,
      role = case$classification$role[[1L]],
      window_id = win$window_id,
      start_iter = win$start_iter,
      end_iter = win$end_iter,
      tau = tau,
      candidate_regime =
        cnd$candidate_regime[m_cnd],
      confirmed_regime =
        cnf$confirmed_regime[m_cnf],
      semantics_origin = "existing_article_pipeline",
      stringsAsFactors = FALSE
    )

    q <- q + 1L
  }

  if (!length(rows)) {
    return(
      data.frame(
        case_key = character(),
        experiment = character(),
        stage = character(),
        case_id = character(),
        role = character(),
        window_id = integer(),
        start_iter = numeric(),
        end_iter = numeric(),
        tau = numeric(),
        candidate_regime = character(),
        confirmed_regime = character(),
        semantics_origin = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Existing Phi transition timeline across article cases
#'
#' Boundary locations are mapped to tau by exact original iteration.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_transition_timeline <- function(x) {
  te_validate_article_case_set(x, error = TRUE)

  rows <- list()
  q <- 1L

  for (i in seq_along(x$cases)) {
    case <- x$cases[[i]]

    if (!inherits(
      case$analysis,
      "te_formalspec_analysis"
    )) {
      next
    }

    ev <- DSNeuralRNAS.FormalSpec::dsfs_boundary_events_data(
      case$analysis$boundaries
    )

    if (!nrow(ev)) {
      next
    }

    tau <- .te_case_iter_to_tau(
      case$trace,
      ev$boundary_iter
    )

    rows[[q]] <- data.frame(
      case_key = x$case_keys[[i]],
      experiment = case$experiment,
      stage = case$stage,
      case_id = case$case_id,
      role = case$classification$role[[1L]],
      boundary_id = ev$boundary_id,
      window_id = ev$window_id,
      boundary_iter = ev$boundary_iter,
      tau = tau,
      transition = ev$transition,
      from_regime = ev$from_regime,
      to_regime = ev$to_regime,
      boundary_type = ev$boundary_type,
      localization = ev$localization,
      semantics_origin = "existing_article_pipeline",
      stringsAsFactors = FALSE
    )

    q <- q + 1L
  }

  if (!length(rows)) {
    return(
      data.frame(
        case_key = character(),
        experiment = character(),
        stage = character(),
        case_id = character(),
        role = character(),
        boundary_id = integer(),
        window_id = integer(),
        boundary_iter = numeric(),
        tau = numeric(),
        transition = character(),
        from_regime = character(),
        to_regime = character(),
        boundary_type = character(),
        localization = character(),
        semantics_origin = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Descriptive semantic occupancy by case
#'
#' Counts windows in each confirmed regime. This is not a ranking or score.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_regime_occupancy <- function(x) {
  d <- te_article_case_set_semantic_timeline(x)

  if (!nrow(d)) {
    return(
      data.frame(
        case_key = character(),
        experiment = character(),
        stage = character(),
        case_id = character(),
        confirmed_regime = character(),
        n_windows = integer(),
        fraction_windows = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  key <- interaction(
    d$case_key,
    d$confirmed_regime,
    drop = TRUE,
    lex.order = TRUE
  )

  parts <- split(d, key)

  rows <- lapply(parts, function(g) {
    total <- sum(d$case_key == g$case_key[[1L]])

    data.frame(
      case_key = g$case_key[[1L]],
      experiment = g$experiment[[1L]],
      stage = g$stage[[1L]],
      case_id = g$case_id[[1L]],
      confirmed_regime = g$confirmed_regime[[1L]],
      n_windows = nrow(g),
      fraction_windows = nrow(g) / total,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[
    order(out$case_key, out$confirmed_regime),
    ,
    drop = FALSE
  ]
}

#' Transition signature for each article case
#'
#' Produces an ordered textual sequence of existing Phi transitions.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_transition_signature <- function(x) {
  ev <- te_article_case_set_transition_timeline(x)

  if (!nrow(ev)) {
    return(
      data.frame(
        case_key = character(),
        experiment = character(),
        stage = character(),
        case_id = character(),
        n_transitions = integer(),
        transition_signature = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  parts <- split(
    ev,
    ev$case_key
  )

  rows <- lapply(parts, function(g) {
    g <- g[order(g$boundary_iter), , drop = FALSE]

    steps <- paste0(
      g$transition,
      ":",
      g$from_regime,
      "\u2192",
      g$to_regime,
      "@",
      g$boundary_iter
    )

    data.frame(
      case_key = g$case_key[[1L]],
      experiment = g$experiment[[1L]],
      stage = g$stage[[1L]],
      case_id = g$case_id[[1L]],
      n_transitions = nrow(g),
      transition_signature = paste(
        steps,
        collapse = " | "
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
