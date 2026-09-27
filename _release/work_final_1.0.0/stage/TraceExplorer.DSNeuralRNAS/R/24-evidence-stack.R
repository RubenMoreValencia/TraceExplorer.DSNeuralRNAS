#' Five-level DSNeuralRNAS evidence contract
#'
#' @return data.frame.
#' @export
te_evidence_level_contract <- function() {
  data.frame(
    level = 1:5,
    key = c(
      "observed_data",
      "final_result",
      "learning_trace",
      "formalspec_evidence",
      "analytical_reference"
    ),
    name = c(
      "Datos observados",
      "Resultado final",
      "Trayectoria de aprendizaje",
      "Evidencia FormalSpec",
      "Referencia anal\u00edtica"
    ),
    primary_question = c(
      "\u00bfQu\u00e9 problema, datos o se\u00f1ales originan el aprendizaje?",
      "\u00bfQu\u00e9 resultado final produjo el modelo?",
      "\u00bfC\u00f3mo evolucion\u00f3 internamente el aprendizaje?",
      "\u00bfQu\u00e9 reg\u00edmenes persistentes y fronteras describe FormalSpec?",
      "\u00bfExiste una verdad anal\u00edtica conocida para contrastar la detecci\u00f3n?"
    ),
    formal_role = c(
      "Experimental context",
      "Applied outcome",
      "Primary dynamic evidence",
      "Trace-derived formal evidence",
      "Controlled validation reference"
    ),
    required_for_dynamic_formalspec = c(
      FALSE, FALSE, TRUE, FALSE, FALSE
    ),
    optional = c(
      FALSE, FALSE, FALSE, TRUE, TRUE
    ),
    didactic_rule = c(
      "Contextualizar antes de interpretar la traza.",
      "Distinguir utilidad final de historia de aprendizaje.",
      "Observar se\u00f1ales antes de clasificar reg\u00edmenes.",
      "Explicar Psi, Gamma y Phi sin sustituir la traza.",
      "Usar solo cuando la referencia est\u00e9 declarada y sea aplicable."
    ),
    stringsAsFactors = FALSE
  )
}

#' Construct an analytical reference
#'
#' @param type Stable reference type.
#' @param expression Human-readable mathematical expression.
#' @param expected_value Optional scalar expected value.
#' @param parameters Named list of reference parameters.
#' @param scope Named list describing applicability.
#' @param provenance Named list describing origin.
#' @return te_analytical_reference.
#' @export
te_analytical_reference <- function(
    type,
    expression,
    expected_value = NA_real_,
    parameters = list(),
    scope = list(),
    provenance = list()) {

  if (length(type) != 1L || !nzchar(type)) {
    stop("type must be one non-empty string.", call. = FALSE)
  }
  if (length(expression) != 1L || !nzchar(expression)) {
    stop("expression must be one non-empty string.", call. = FALSE)
  }

  out <- structure(
    list(
      type = as.character(type),
      expression = as.character(expression),
      expected_value = as.numeric(expected_value),
      parameters = parameters,
      scope = scope,
      provenance = provenance
    ),
    class = "te_analytical_reference"
  )

  te_validate_analytical_reference(out, error = TRUE)
  out
}

#' Validate an analytical reference
#'
#' @param x te_analytical_reference.
#' @param error Stop on failure.
#' @return data.frame.
#' @export
te_validate_analytical_reference <- function(x, error = FALSE) {
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

  is_ref <- inherits(x, "te_analytical_reference")
  add("class", is_ref, "Object must inherit te_analytical_reference.")

  if (is_ref) {
    add(
      "type",
      is.character(x$type) &&
        length(x$type) == 1L &&
        nzchar(x$type),
      "Reference type must be explicit."
    )
    add(
      "expression",
      is.character(x$expression) &&
        length(x$expression) == 1L &&
        nzchar(x$expression),
      "Mathematical expression must be explicit."
    )
    add(
      "expected_value",
      length(x$expected_value) == 1L,
      "Expected value must be one scalar or NA."
    )
    add(
      "scope",
      is.list(x$scope),
      "Applicability scope must be explicit."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Analytical-reference validation failed:",
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

#' Tabular representation of an analytical reference
#'
#' @param x te_analytical_reference.
#' @return data.frame.
#' @export
te_analytical_reference_data <- function(x) {
  te_validate_analytical_reference(x, error = TRUE)

  stringify <- function(v) {
    if (!length(v)) {
      return("")
    }
    paste(
      vapply(
        v,
        function(z) paste(as.character(z), collapse = " | "),
        character(1)
      ),
      collapse = "; "
    )
  }

  data.frame(
    type = x$type,
    expression = x$expression,
    expected_value = x$expected_value,
    parameters = stringify(x$parameters),
    scope = stringify(x$scope),
    provenance = stringify(x$provenance),
    stringsAsFactors = FALSE
  )
}

#' First-article analytical reference for an imported case
#'
#' @param x te_article_case.
#' @return te_analytical_reference or NULL.
#' @export
te_article_analytical_reference <- function(x) {
  if (!inherits(x, "te_article_case")) {
    stop("Expected te_article_case.", call. = FALSE)
  }

  if (!identical(x$experiment, "E1")) {
    return(NULL)
  }

  te_analytical_reference(
    type = "quadratic_stability_boundary",
    expression = "eta_star = 2 / lambda_max(A)",
    expected_value = 0.5,
    parameters = list(
      model = "L(theta) = 1/2 theta^T A theta",
      A = "diag(1, 4)",
      lambda_max_A = 4
    ),
    scope = list(
      article_experiment = "E1",
      applicability = "controlled quadratic reference only"
    ),
    provenance = list(
      source = "DSNeuralRNAS FormalSpec first-article E1 protocol",
      freeze_id = "DSFS-1.0.0-FIRST-ARTICLE"
    )
  )
}

.te_profile_atomic <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.matrix(x) || is.data.frame(x)) {
    cols <- colnames(x)
    if (is.null(cols)) {
      cols <- character()
    }

    return(
      data.frame(
        object = "data",
        class = paste(class(x), collapse = " / "),
        n_observations = nrow(x),
        n_variables = ncol(x),
        detail = paste(head(cols, 12L), collapse = ", "),
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.atomic(x) && !is.list(x)) {
    return(
      data.frame(
        object = "vector",
        class = paste(class(x), collapse = " / "),
        n_observations = length(x),
        n_variables = 1L,
        detail = "",
        stringsAsFactors = FALSE
      )
    )
  }

  NULL
}

#' Conservative observed-data profile
#'
#' @param x te_article_case, te_trace, or producer object.
#' @return data.frame.
#' @export
te_observed_data_profile <- function(x) {
  source <- x

  if (inherits(x, "te_article_case")) {
    source <- x$source_object
  }

  if (inherits(x, "te_trace")) {
    p <- x$provenance
    dataset_id <- if (!is.null(p$dataset_id)) {
      p$dataset_id
    } else {
      "unknown"
    }

    return(
      data.frame(
        object = "provenance_context",
        class = "te_trace",
        n_observations = NA_integer_,
        n_variables = NA_integer_,
        detail = paste0("dataset_id=", dataset_id),
        stringsAsFactors = FALSE
      )
    )
  }

  rows <- list()
  q <- 1L

  if (is.list(source)) {
    known <- c(
      "X", "x", "y", "data", "datos",
      "signals", "signal", "series", "serie"
    )

    for (nm in intersect(known, names(source))) {
      item <- .te_profile_atomic(source[[nm]])
      if (!is.null(item)) {
        item$object <- nm
        rows[[q]] <- item
        q <- q + 1L
      }
    }
  }

  if (!length(rows)) {
    return(
      data.frame(
        object = character(),
        class = character(),
        n_observations = integer(),
        n_variables = integer(),
        detail = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.te_final_result_from_trace <- function(trace) {
  if (!inherits(trace, "te_trace")) {
    return(
      data.frame(
        metric = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  d <- te_trace_data(trace)
  l0 <- d$loss[[1L]]
  lf <- d$loss[[nrow(d)]]

  reduction <- if (
    is.finite(l0) &&
    abs(l0) > .Machine$double.eps
  ) {
    (l0 - lf) / abs(l0)
  } else {
    NA_real_
  }

  data.frame(
    metric = c(
      "n_states",
      "loss_initial",
      "loss_final",
      "loss_change",
      "relative_loss_reduction"
    ),
    value = c(
      nrow(d),
      l0,
      lf,
      lf - l0,
      reduction
    ),
    stringsAsFactors = FALSE
  )
}


#' Build the five-level evidence stack
#'
#' @param x te_article_case or te_trace.
#' @param analytical_reference Optional explicit te_analytical_reference.
#' @return te_evidence_stack.
#' @export
te_evidence_stack <- function(
    x,
    analytical_reference = NULL) {

  is_article <- inherits(x, "te_article_case")
  is_trace <- inherits(x, "te_trace")

  if (!is_article && !is_trace) {
    stop(
      "x must be te_article_case or te_trace.",
      call. = FALSE
    )
  }

  trace <- if (is_article) x$trace else x
  analysis <- if (is_article) x$analysis else NULL

  observed <- te_observed_data_profile(x)
  final <- .te_final_result_from_trace(trace)

  if (is.null(analytical_reference) && is_article) {
    analytical_reference <- te_article_analytical_reference(x)
  }

  if (
    !is.null(analytical_reference) &&
    !inherits(analytical_reference, "te_analytical_reference")
  ) {
    stop(
      "analytical_reference must be te_analytical_reference or NULL.",
      call. = FALSE
    )
  }

  contract <- te_evidence_level_contract()

  raw_context <- nrow(observed) > 0L
  has_trace <- inherits(trace, "te_trace")
  has_analysis <- inherits(
    analysis,
    "te_formalspec_analysis"
  )
  has_reference <- inherits(
    analytical_reference,
    "te_analytical_reference"
  )
  has_final <- nrow(final) > 0L

  dataset_context <- FALSE
  if (has_trace) {
    dataset_context <- !is.null(trace$provenance$dataset_id)
  }

  availability <- c(
    if (raw_context) {
      "available"
    } else if (dataset_context) {
      "partial"
    } else {
      "unavailable"
    },
    if (has_final) "available" else "unavailable",
    if (has_trace) "available" else "unavailable",
    if (has_analysis) {
      "available_existing"
    } else if (has_trace) {
      "eligible_not_existing"
    } else {
      "unavailable"
    },
    if (has_reference) "available" else "not_applicable"
  )

  evidence_state <- c(
    if (raw_context) {
      "source_data_present"
    } else if (dataset_context) {
      "context_only"
    } else {
      "absent"
    },
    if (has_final) "trace_summary" else "absent",
    if (has_trace) "canonical_trace" else "absent",
    if (has_analysis) {
      "existing_formalspec_analysis"
    } else if (has_trace) {
      "trace_eligible"
    } else {
      "absent"
    },
    if (has_reference) analytical_reference$type else "none_declared"
  )

  implementation_rule <- c(
    "Never reconstruct raw observations from the trace.",
    "Do not treat final metrics as a substitute for learning history.",
    "Required before dynamic FormalSpec interpretation.",
    "Reuse existing article semantics when present; do not silently recalibrate.",
    "Overlay only when applicability is explicitly declared."
  )

  levels <- cbind(
    contract,
    availability = availability,
    evidence_state = evidence_state,
    implementation_rule = implementation_rule,
    stringsAsFactors = FALSE
  )

  out <- structure(
    list(
      levels = levels,
      observed_data = observed,
      final_result = final,
      learning_trace = trace,
      formalspec_analysis = analysis,
      analytical_reference = analytical_reference,
      source = x,
      stack_version = "0.10.0",
      ecosystem = "DSNeuralRNAS"
    ),
    class = "te_evidence_stack"
  )

  te_validate_evidence_stack(out, error = TRUE)
  out
}

#' Validate an evidence stack
#'
#' @param x te_evidence_stack.
#' @param error Stop on failure.
#' @return data.frame.
#' @export
te_validate_evidence_stack <- function(x, error = FALSE) {
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

  is_stack <- inherits(x, "te_evidence_stack")

  add(
    "class",
    is_stack,
    "Object must inherit te_evidence_stack."
  )

  if (is_stack) {
    add(
      "five_levels",
      is.data.frame(x$levels) &&
        nrow(x$levels) == 5L &&
        identical(x$levels$level, 1:5),
      "Evidence stack must preserve five ordered levels."
    )

    add(
      "trace_consistency",
      !inherits(x$learning_trace, "te_trace") ||
        nrow(te_trace_data(x$learning_trace)) >= 2L,
      "A present learning trace must satisfy the canonical trace contract."
    )

    add(
      "formalspec_consistency",
      !inherits(
        x$formalspec_analysis,
        "te_formalspec_analysis"
      ) ||
        inherits(x$learning_trace, "te_trace"),
      "FormalSpec evidence cannot exist without a learning trace."
    )

    add(
      "analytical_reference_consistency",
      is.null(x$analytical_reference) ||
        inherits(
          x$analytical_reference,
          "te_analytical_reference"
        ),
      "Analytical reference must be explicit or absent."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Evidence-stack validation failed:",
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

#' Evidence-stack level table
#'
#' @param x te_evidence_stack.
#' @return data.frame.
#' @export
te_evidence_stack_levels <- function(x) {
  te_validate_evidence_stack(x, error = TRUE)
  x$levels
}

#' Implementation decisions derived from evidence availability
#'
#' @param x te_evidence_stack.
#' @return data.frame.
#' @export
te_evidence_stack_decisions <- function(x) {
  te_validate_evidence_stack(x, error = TRUE)

  has_trace <- inherits(x$learning_trace, "te_trace")
  has_fs <- inherits(
    x$formalspec_analysis,
    "te_formalspec_analysis"
  )
  has_ref <- inherits(
    x$analytical_reference,
    "te_analytical_reference"
  )

  data.frame(
    decision = c(
      "show_data_context",
      "show_final_result",
      "enable_trace_views",
      "reuse_existing_formalspec",
      "allow_new_frozen_formalspec_analysis",
      "enable_semantic_comparison",
      "enable_analytical_reference_overlay",
      "infer_missing_trace",
      "automatic_learning_policy"
    ),
    status = c(
      if (nrow(x$observed_data)) "SUPPORTED" else "LIMITED",
      if (nrow(x$final_result)) "SUPPORTED" else "UNAVAILABLE",
      if (has_trace) "SUPPORTED" else "UNAVAILABLE",
      if (has_fs) "SUPPORTED" else "UNAVAILABLE",
      if (has_trace && !has_fs) {
        "SUPPORTED_EXPLICIT"
      } else if (has_fs) {
        "NOT_NEEDED"
      } else {
        "UNAVAILABLE"
      },
      if (has_fs) "SUPPORTED" else "UNAVAILABLE",
      if (has_ref) "SUPPORTED" else "NOT_APPLICABLE",
      "PROHIBITED",
      "OUT_OF_SCOPE"
    ),
    reason = c(
      "Use only source data actually present; provenance-only context is partial.",
      "Final metrics summarize utility but do not replace trajectory evidence.",
      "2D/3D and temporal inspection require a canonical trace.",
      "Existing article Psi/Gamma/Phi must be reused without silent reclassification.",
      "A new analysis may be run explicitly with the frozen FormalSpec protocol.",
      "Semantic comparison requires existing FormalSpec evidence.",
      "Analytical overlay requires an explicit applicability contract.",
      "TraceExplorer must not invent temporal evidence.",
      "TraceExplorer observes, explains and traces; it is not yet a control policy engine."
    ),
    stringsAsFactors = FALSE
  )
}

#' Extension points for future ecosystems
#'
#' @return data.frame.
#' @export
te_evidence_extension_points <- function() {
  data.frame(
    extension_point = c(
      "observed_data_adapter",
      "final_result_adapter",
      "trace_bridge",
      "semantic_provider",
      "analytical_reference_provider",
      "decision_policy_provider"
    ),
    current_dsneural_role = c(
      "Profile X/y/signals only when present.",
      "Trace-derived metrics and producer summaries.",
      "Canonical te_trace adapters and bridges.",
      "Frozen DSNeuralRNAS.FormalSpec analysis.",
      "E1 quadratic analytical reference.",
      "Reserved; not implemented as control."
    ),
    future_scalability = c(
      "Other data modalities may provide their own profile contract.",
      "Domain applications may add outcome summaries.",
      "Other learning engines may map to the observable trace contract.",
      "Other formal systems may implement a declared semantic provider.",
      "Other experiments may register explicit analytical or benchmark references.",
      "Future policy/control layers must remain separate from evidence observation."
    ),
    stringsAsFactors = FALSE
  )
}

#' Compare a trace with an analytical reference when supported
#'
#' @param x te_evidence_stack.
#' @return data.frame.
#' @export
te_evidence_analytical_contrast <- function(x) {
  te_validate_evidence_stack(x, error = TRUE)

  ref <- x$analytical_reference
  tr <- x$learning_trace

  if (
    !inherits(ref, "te_analytical_reference") ||
    !inherits(tr, "te_trace")
  ) {
    return(
      data.frame(
        metric = character(),
        value = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  if (!identical(
    ref$type,
    "quadratic_stability_boundary"
  )) {
    return(
      data.frame(
        metric = "unsupported_reference_type",
        value = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  d <- te_trace_data(tr)

  if (!"eta" %in% names(d)) {
    return(
      data.frame(
        metric = "eta_unavailable",
        value = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  eta_star <- ref$expected_value
  eta <- d$eta
  finite <- is.finite(eta)

  if (!any(finite)) {
    return(
      data.frame(
        metric = "eta_unavailable",
        value = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  above <- eta > eta_star
  equal <- abs(eta - eta_star) <= 1e-12

  first_above <- if (any(above, na.rm = TRUE)) {
    d$iter[which(above)[[1L]]]
  } else {
    NA_real_
  }

  data.frame(
    metric = c(
      "eta_star",
      "eta_min",
      "eta_max",
      "n_below",
      "n_equal",
      "n_above",
      "first_above_iter"
    ),
    value = c(
      eta_star,
      min(eta[finite]),
      max(eta[finite]),
      sum(eta < eta_star, na.rm = TRUE),
      sum(equal, na.rm = TRUE),
      sum(above, na.rm = TRUE),
      first_above
    ),
    stringsAsFactors = FALSE
  )
}
