#' Preflight for the frozen FormalSpec article baseline
#'
#' TraceExplorer v0.2.0 treats DSNeuralRNAS.FormalSpec 1.0.1 with formal
#' specification 1.0.0 as the reproducible baseline used by the first article.
#'
#' @param required_package_version Expected installed package version.
#' @param required_spec_version Expected formal specification version.
#' @param error Stop if a required check fails.
#' @return A data.frame of checks.
#' @export
te_formalspec_preflight <- function(
    required_package_version = "1.0.1",
    required_spec_version = "1.0.0",
    error = TRUE) {

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

  installed <- requireNamespace(
    "DSNeuralRNAS.FormalSpec",
    quietly = TRUE
  )
  add(
    "package_installed",
    installed,
    if (installed) {
      "DSNeuralRNAS.FormalSpec is installed."
    } else {
      "DSNeuralRNAS.FormalSpec is not installed."
    }
  )

  if (!installed) {
    if (isTRUE(error)) {
      stop(
        "FormalSpec preflight failed: package not installed.",
        call. = FALSE
      )
    }
    return(checks)
  }

  pkg_version <- as.character(
    utils::packageVersion("DSNeuralRNAS.FormalSpec")
  )
  add(
    "package_version",
    identical(pkg_version, required_package_version),
    paste0(
      "Installed package version: ", pkg_version,
      "; TraceExplorer baseline: ", required_package_version, "."
    )
  )

  required_exports <- c(
    "dsfs_spec",
    "dsfs_metadata",
    "dsfs_trace",
    "dsfs_trace_data",
    "dsfs_validate_trace",
    "dsfs_derive_observables",
    "dsfs_validate_observables",
    "dsfs_build_windows",
    "dsfs_validate_windows",
    "dsfs_windows_data",
    "dsfs_classify_regimes",
    "dsfs_validate_regime_candidates",
    "dsfs_regime_candidates_data",
    "dsfs_confirm_regimes",
    "dsfs_validate_confirmed_regimes",
    "dsfs_confirmed_regimes_data",
    "dsfs_detect_boundaries",
    "dsfs_validate_boundaries",
    "dsfs_boundary_events_data",
    "dsfs_article_protocol"
  )
  exports <- getNamespaceExports("DSNeuralRNAS.FormalSpec")
  missing <- setdiff(required_exports, exports)
  add(
    "required_exports",
    length(missing) == 0L,
    if (length(missing) == 0L) {
      "All required FormalSpec exports are available."
    } else {
      paste0(
        "Missing exports: ",
        paste(missing, collapse = ", "),
        "."
      )
    }
  )

  spec <- DSNeuralRNAS.FormalSpec::dsfs_spec()
  spec_version <- spec$version
  add(
    "formal_spec_version",
    identical(spec_version, required_spec_version),
    paste0(
      "Active formal specification: ", spec_version,
      "; required baseline: ", required_spec_version, "."
    )
  )

  protocol_ok <- FALSE
  protocol_detail <- "Frozen article protocol could not be materialized."
  protocol <- tryCatch(
    DSNeuralRNAS.FormalSpec::dsfs_article_protocol(spec),
    error = function(e) e
  )
  if (!inherits(protocol, "error")) {
    protocol_ok <- inherits(protocol, "dsfs_article_protocol") &&
      identical(protocol$spec$version, required_spec_version)
    protocol_detail <- paste0(
      "Frozen protocol available: ",
      protocol$protocol_id,
      "; window_width=", protocol$window_width,
      "; zero_tol=", protocol$zero_tol,
      "."
    )
  } else {
    protocol_detail <- conditionMessage(protocol)
  }
  add(
    "article_protocol",
    protocol_ok,
    protocol_detail
  )

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "FormalSpec preflight failed:",
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

#' Frozen FormalSpec protocol used by TraceExplorer
#'
#' @export
te_formalspec_protocol <- function() {
  te_formalspec_preflight(error = TRUE)
  spec <- DSNeuralRNAS.FormalSpec::dsfs_spec()
  DSNeuralRNAS.FormalSpec::dsfs_article_protocol(spec)
}

.te_scalar_character <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(fallback)
  }
  y <- as.character(x[[1L]])
  if (!nzchar(y)) fallback else y
}

.te_trace_formalspec_metadata <- function(trace) {
  p <- trace$provenance

  DSNeuralRNAS.FormalSpec::dsfs_metadata(
    experiment_id = .te_scalar_character(
      p$experiment_id,
      "traceexplorer-session"
    ),
    engine = .te_scalar_character(
      p$source_package,
      "TraceExplorer.DSNeuralRNAS"
    ),
    engine_version = .te_scalar_character(
      p$source_package_version,
      "unknown"
    ),
    seed = if (
      is.null(p$seed) ||
      length(p$seed) == 0L ||
      all(is.na(p$seed))
    ) {
      NA_integer_
    } else {
      as.integer(p$seed[[1L]])
    },
    dataset_id = .te_scalar_character(
      p$dataset_id,
      "traceexplorer-source"
    ),
    model_id = .te_scalar_character(
      p$model_id,
      .te_scalar_character(
        p$source_class,
        "unknown-model"
      )
    ),
    optimizer = .te_scalar_character(
      p$optimizer,
      "source_defined"
    ),
    notes = paste0(
      "TraceExplorer.DSNeuralRNAS bridge; adapter_mode=",
      .te_scalar_character(p$adapter_mode, "unknown"),
      "; legacy semantics promoted=FALSE."
    )
  )
}

#' Convert a TraceExplorer trace to a frozen FormalSpec trace
#'
#' @param trace te_trace.
#' @param metadata Optional dsfs_metadata. If NULL, conservative explicit
#' metadata are generated from TraceExplorer provenance.
#' @return dsfs_trace.
#' @export
te_to_formalspec <- function(trace, metadata = NULL) {
  if (!inherits(trace, "te_trace")) {
    stop("Expected object of class te_trace.", call. = FALSE)
  }

  te_formalspec_preflight(error = TRUE)
  protocol <- te_formalspec_protocol()

  d <- te_trace_data(trace)

  if (is.null(metadata)) {
    metadata <- .te_trace_formalspec_metadata(trace)
  }

  out <- DSNeuralRNAS.FormalSpec::dsfs_trace(
    d,
    metadata = metadata,
    spec = protocol$spec
  )

  DSNeuralRNAS.FormalSpec::dsfs_validate_trace(
    out,
    error = TRUE
  )
  out
}

#' Run Trace -> Observables -> Windows -> Psi -> Gamma -> Phi
#'
#' The frozen first-article thresholds and persistence rules are reused
#' without calibration on the loaded trace.
#'
#' @param trace te_trace.
#' @return te_formalspec_analysis.
#' @export
te_run_formalspec <- function(trace) {
  protocol <- te_formalspec_protocol()
  fs_trace <- te_to_formalspec(trace)

  observables <-
    DSNeuralRNAS.FormalSpec::dsfs_derive_observables(fs_trace)

  windows <- DSNeuralRNAS.FormalSpec::dsfs_build_windows(
    observables,
    width = protocol$window_width,
    zero_tol = protocol$zero_tol
  )

  candidates <- DSNeuralRNAS.FormalSpec::dsfs_classify_regimes(
    windows,
    protocol$regime_thresholds
  )

  confirmed <- DSNeuralRNAS.FormalSpec::dsfs_confirm_regimes(
    candidates,
    protocol$persistence_rules
  )

  boundaries <- DSNeuralRNAS.FormalSpec::dsfs_detect_boundaries(
    confirmed
  )

  out <- structure(
    list(
      trace = fs_trace,
      observables = observables,
      windows = windows,
      candidates = candidates,
      confirmed = confirmed,
      boundaries = boundaries,
      protocol = protocol,
      calibration_on_trace = FALSE,
      freeze_id = "DSFS-1.0.0-FIRST-ARTICLE"
    ),
    class = "te_formalspec_analysis"
  )

  te_validate_formalspec_analysis(out, error = TRUE)
  out
}

#' Validate a complete FormalSpec analysis
#'
#' @param x te_formalspec_analysis.
#' @param error Stop when invalid.
#' @return data.frame.
#' @export
te_validate_formalspec_analysis <- function(x, error = FALSE) {
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

  ok_class <- inherits(x, "te_formalspec_analysis")
  add(
    "class",
    ok_class,
    "Object must inherit te_formalspec_analysis."
  )

  if (ok_class) {
    validators <- list(
      trace = DSNeuralRNAS.FormalSpec::dsfs_validate_trace,
      observables = DSNeuralRNAS.FormalSpec::dsfs_validate_observables,
      windows = DSNeuralRNAS.FormalSpec::dsfs_validate_windows,
      candidates =
        DSNeuralRNAS.FormalSpec::dsfs_validate_regime_candidates,
      confirmed =
        DSNeuralRNAS.FormalSpec::dsfs_validate_confirmed_regimes,
      boundaries =
        DSNeuralRNAS.FormalSpec::dsfs_validate_boundaries
    )

    for (nm in names(validators)) {
      val <- validators[[nm]](x[[nm]])
      add(
        paste0(nm, "_valid"),
        isTRUE(val$valid),
        paste0(nm, " validation via FormalSpec.")
      )
    }

    add(
      "frozen_protocol",
      inherits(x$protocol, "dsfs_article_protocol") &&
        identical(x$protocol$spec$version, "1.0.0"),
      "FormalSpec 1.0.0 frozen article protocol must be used."
    )

    add(
      "no_calibration",
      identical(x$calibration_on_trace, FALSE),
      "TraceExplorer must not recalibrate thresholds on the loaded trace."
    )

    add(
      "freeze_id",
      identical(x$freeze_id, "DSFS-1.0.0-FIRST-ARTICLE"),
      "First-article freeze identifier must be preserved."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "FormalSpec analysis validation failed:",
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

#' Summarize a complete FormalSpec analysis
#'
#' @param x te_formalspec_analysis.
#' @return One-row data.frame.
#' @export
te_formalspec_summary <- function(x) {
  te_validate_formalspec_analysis(x, error = TRUE)

  tr <- DSNeuralRNAS.FormalSpec::dsfs_trace_data(x$trace)
  win <- DSNeuralRNAS.FormalSpec::dsfs_windows_data(x$windows)
  cnd <- DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
    x$candidates
  )
  cnf <- DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
    x$confirmed
  )
  ev <- DSNeuralRNAS.FormalSpec::dsfs_boundary_events_data(
    x$boundaries
  )

  data.frame(
    formal_spec_version = x$protocol$spec$version,
    freeze_id = x$freeze_id,
    n_trace_rows = nrow(tr),
    n_windows = nrow(win),
    n_candidate_regimes =
      length(unique(cnd$candidate_regime)),
    n_confirmed_regimes =
      length(unique(cnf$confirmed_regime)),
    n_boundary_events = nrow(ev),
    first_boundary_iter =
      if (nrow(ev)) ev$boundary_iter[[1L]] else NA_real_,
    calibration_on_trace = x$calibration_on_trace,
    stringsAsFactors = FALSE
  )
}
