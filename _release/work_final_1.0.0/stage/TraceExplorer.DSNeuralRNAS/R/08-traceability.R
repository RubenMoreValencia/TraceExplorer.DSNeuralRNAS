#' Build a traceability index
#'
#' @param trace te_trace.
#' @param analysis Optional te_formalspec_analysis.
#' @return data.frame suitable for point inspection.
#' @export
te_traceability_index <- function(trace, analysis = NULL) {
  d <- te_trace_data(trace)
  out <- data.frame(
    row_id = seq_len(nrow(d)),
    iter = d$iter,
    loss = d$loss,
    stringsAsFactors = FALSE
  )

  for (nm in intersect(
    c(
      "delta_loss",
      "legacy_delta_loss",
      "grad_norm",
      "eta",
      "param_norm",
      "param_velocity"
    ),
    names(d)
  )) {
    out[[nm]] <- d[[nm]]
  }

  out$window_id <- NA_integer_
  out$candidate_regime <- NA_character_
  out$confirmed_regime <- NA_character_
  out$boundary_type <- NA_character_

  if (!is.null(analysis)) {
    te_validate_formalspec_analysis(
      analysis,
      error = TRUE
    )

    w <- DSNeuralRNAS.FormalSpec::dsfs_windows_data(
      analysis$windows
    )
    cnd <- DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
      analysis$candidates
    )
    cnf <- DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
      analysis$confirmed
    )
    ev <- DSNeuralRNAS.FormalSpec::dsfs_boundary_events_data(
      analysis$boundaries
    )

    m0 <- match(w$end_iter, out$iter)
    good0 <- !is.na(m0)
    out$window_id[m0[good0]] <- w$window_id[good0]

    m1 <- match(cnd$end_iter, out$iter)
    good1 <- !is.na(m1)
    out$candidate_regime[m1[good1]] <-
      cnd$candidate_regime[good1]

    m2 <- match(cnf$end_iter, out$iter)
    good2 <- !is.na(m2)
    out$confirmed_regime[m2[good2]] <-
      cnf$confirmed_regime[good2]

    if (nrow(ev) > 0L) {
      m3 <- match(ev$boundary_iter, out$iter)
      good3 <- !is.na(m3)
      out$boundary_type[m3[good3]] <-
        ev$boundary_type[good3]
    }
  }

  p <- trace$provenance
  out$source_package <-
    .te_scalar_character(p$source_package, NA_character_)
  out$source_class <-
    .te_scalar_character(p$source_class, NA_character_)
  out$adapter_mode <-
    .te_scalar_character(p$adapter_mode, NA_character_)
  out
}
