#' Apply the frozen FormalSpec pipeline independently to each run
#'
#' No threshold is recalibrated and no semantic state is pooled between runs.
#'
#' @param x te_multirun.
#' @return te_multirun_formalspec.
#' @export
te_multirun_formalspec <- function(x) {
  te_validate_multirun(x, error = TRUE)

  analyses <- lapply(
    x$traces,
    te_run_formalspec
  )
  names(analyses) <- x$run_ids

  out <- structure(
    list(
      analyses = analyses,
      run_ids = x$run_ids,
      labels = x$labels,
      freeze_id = "DSFS-1.0.0-FIRST-ARTICLE",
      calibration_on_runs = FALSE
    ),
    class = "te_multirun_formalspec"
  )

  te_validate_multirun_formalspec(out, error = TRUE)
  out
}

#' Validate multi-run FormalSpec results
#'
#' @param x te_multirun_formalspec.
#' @param error Stop on failure.
#' @return Validation table.
#' @export
te_validate_multirun_formalspec <- function(x, error = FALSE) {
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

  ok_class <- inherits(x, "te_multirun_formalspec")
  add(
    "class",
    ok_class,
    "Object must inherit te_multirun_formalspec."
  )

  if (ok_class) {
    vals <- vapply(
      x$analyses,
      function(a) {
        v <- te_validate_formalspec_analysis(a)
        all(v$status == "PASS")
      },
      logical(1)
    )

    add(
      "all_analyses_valid",
      all(vals),
      "Every run must independently pass FormalSpec validation."
    )
    add(
      "freeze_id",
      identical(
        x$freeze_id,
        "DSFS-1.0.0-FIRST-ARTICLE"
      ),
      "The first-article freeze must be shared by all runs."
    )
    add(
      "no_calibration",
      identical(x$calibration_on_runs, FALSE),
      "No multi-run calibration is permitted in this phase."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Multi-run FormalSpec validation failed:",
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

#' FormalSpec summary across runs
#'
#' @param x te_multirun_formalspec.
#' @return data.frame.
#' @export
te_multirun_formalspec_summary <- function(x) {
  te_validate_multirun_formalspec(x, error = TRUE)

  rows <- lapply(seq_along(x$analyses), function(i) {
    s <- te_formalspec_summary(x$analyses[[i]])
    cbind(
      data.frame(
        run_id = x$run_ids[[i]],
        run_label = x$labels[[i]],
        stringsAsFactors = FALSE
      ),
      s,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' FormalSpec semantic data across runs
#'
#' Returns candidate and confirmed regimes at each window endpoint, plus
#' boundary type when a boundary occurs.
#'
#' @param x te_multirun_formalspec.
#' @return data.frame.
#' @export
te_multirun_semantic_data <- function(x) {
  te_validate_multirun_formalspec(x, error = TRUE)

  rows <- lapply(seq_along(x$analyses), function(i) {
    a <- x$analyses[[i]]

    cnd <- DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
      a$candidates
    )
    cnf <- DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
      a$confirmed
    )
    ev <- DSNeuralRNAS.FormalSpec::dsfs_boundary_events_data(
      a$boundaries
    )

    d <- data.frame(
      run_id = x$run_ids[[i]],
      run_label = x$labels[[i]],
      window_id = cnd$window_id,
      end_iter = cnd$end_iter,
      candidate_regime = cnd$candidate_regime,
      confirmed_regime = cnf$confirmed_regime,
      boundary_type = NA_character_,
      stringsAsFactors = FALSE
    )

    if (nrow(ev) > 0L) {
      m <- match(ev$boundary_iter, d$end_iter)
      good <- !is.na(m)
      d$boundary_type[m[good]] <- ev$boundary_type[good]
    }

    d
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
