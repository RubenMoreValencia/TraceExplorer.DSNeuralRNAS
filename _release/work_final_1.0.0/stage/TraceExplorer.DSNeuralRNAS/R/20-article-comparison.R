#' Create a scientific comparison set from article cases
#'
#' Cases retain their own provenance and FormalSpec evidence. The set does not
#' pool, average, or recalibrate semantic states.
#'
#' @param cases List of te_article_case objects.
#' @param comparison_id Stable identifier.
#' @return te_article_case_set.
#' @export
te_article_case_set <- function(
    cases,
    comparison_id = "article-comparison") {

  if (!is.list(cases) || length(cases) < 1L) {
    stop("cases must be a non-empty list.", call. = FALSE)
  }

  ok <- vapply(
    cases,
    inherits,
    logical(1),
    what = "te_article_case"
  )
  if (!all(ok)) {
    stop(
      "Every element must inherit te_article_case.",
      call. = FALSE
    )
  }

  keys <- vapply(
    cases,
    function(x) {
      paste(
        .te_article_scalar(x$experiment, "EX"),
        .te_article_scalar(x$stage, "stage"),
        .te_article_scalar(x$case_id, "case"),
        .te_article_scalar(
          x$classification$role[[1L]],
          "role"
        ),
        sep = "::"
      )
    },
    character(1)
  )

  if (anyDuplicated(keys)) {
    keys <- make.unique(keys, sep = "::")
  }

  names(cases) <- keys

  out <- structure(
    list(
      cases = cases,
      case_keys = keys,
      comparison_id = as.character(comparison_id),
      contract_version = "0.8.0"
    ),
    class = "te_article_case_set"
  )

  te_validate_article_case_set(out, error = TRUE)
  out
}

#' Validate an article comparison set
#'
#' @param x te_article_case_set.
#' @param error Stop on failure.
#' @return Validation table.
#' @export
te_validate_article_case_set <- function(x, error = FALSE) {
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

  is_set <- inherits(x, "te_article_case_set")

  add(
    "class",
    is_set,
    "Object must inherit te_article_case_set."
  )

  if (is_set) {
    add(
      "non_empty",
      length(x$cases) >= 1L,
      "At least one article case is required."
    )

    add(
      "all_cases",
      all(vapply(
        x$cases,
        inherits,
        logical(1),
        what = "te_article_case"
      )),
      "Every element must preserve te_article_case."
    )

    add(
      "case_keys",
      length(x$case_keys) == length(x$cases) &&
        !anyDuplicated(x$case_keys),
      "Case keys must be complete and unique."
    )

    md5_ok <- vapply(
      x$cases,
      function(case) {
        is.character(case$source_md5) &&
          length(case$source_md5) == 1L &&
          (
            is.na(case$source_md5) ||
              nchar(case$source_md5) == 32L
          )
      },
      logical(1)
    )

    add(
      "source_integrity",
      all(md5_ok),
      "Each case must preserve source MD5 when file-backed."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Article case-set validation failed:",
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

#' Load multiple article cases from exact catalog rows
#'
#' This function does not apply role preference. The selected catalog rows are
#' loaded exactly as supplied.
#'
#' @param catalog Output of te_article_catalog().
#' @param rows Integer row indices.
#' @param comparison_id Stable identifier.
#' @return te_article_case_set.
#' @export
te_article_load_case_set <- function(
    catalog,
    rows,
    comparison_id = "article-comparison") {

  if (!is.data.frame(catalog)) {
    stop("catalog must be a data.frame.", call. = FALSE)
  }

  rows <- as.integer(rows)

  if (
    !length(rows) ||
    any(!is.finite(rows)) ||
    any(rows < 1L) ||
    any(rows > nrow(catalog))
  ) {
    stop(
      "rows must contain valid catalog row indices.",
      call. = FALSE
    )
  }

  cases <- lapply(
    rows,
    function(i) {
      te_article_load_rds(
        catalog$path[[i]]
      )
    }
  )

  te_article_case_set(
    cases,
    comparison_id = comparison_id
  )
}

#' Descriptive index for article cases
#'
#' This function is descriptive. It does not rank cases or choose a winner.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_index <- function(x) {
  te_validate_article_case_set(x, error = TRUE)

  rows <- lapply(seq_along(x$cases), function(i) {
    case <- x$cases[[i]]
    s <- te_article_case_summary(case)
    st <- te_article_display_state(case)

    loss_initial <- NA_real_
    loss_final <- NA_real_
    relative_loss_reduction <- NA_real_

    if (inherits(case$trace, "te_trace")) {
      d <- te_trace_data(case$trace)
      loss_initial <- d$loss[[1L]]
      loss_final <- d$loss[[nrow(d)]]

      if (
        is.finite(loss_initial) &&
        abs(loss_initial) > .Machine$double.eps
      ) {
        relative_loss_reduction <-
          (loss_initial - loss_final) /
          abs(loss_initial)
      }
    }

    data.frame(
      case_key = x$case_keys[[i]],
      experiment = s$experiment[[1L]],
      stage = s$stage[[1L]],
      case_id = s$case_id[[1L]],
      role = s$role[[1L]],
      display_state = st$state[[1L]],
      source_name = s$source_name[[1L]],
      source_md5 = s$source_md5[[1L]],
      n_states = s$n_states[[1L]],
      n_windows = s$n_windows[[1L]],
      n_boundary_events = s$n_boundary_events[[1L]],
      existing_article_semantics =
        s$existing_article_semantics[[1L]],
      loss_initial = loss_initial,
      loss_final = loss_final,
      relative_loss_reduction =
        relative_loss_reduction,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Combined trace data for dynamic article cases
#'
#' Tau is a visualization coordinate only. Original iterations and case
#' identity are retained and no interpolation is performed.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_trace_data <- function(x) {
  te_validate_article_case_set(x, error = TRUE)

  rows <- list()
  q <- 1L

  for (i in seq_along(x$cases)) {
    case <- x$cases[[i]]

    if (!inherits(case$trace, "te_trace")) {
      next
    }

    d <- te_trace_data(case$trace)

    rows[[q]] <- data.frame(
      case_key = x$case_keys[[i]],
      experiment = case$experiment,
      stage = case$stage,
      case_id = case$case_id,
      role = case$classification$role[[1L]],
      iter = d$iter,
      tau = te_normalized_time(case$trace),
      loss = d$loss,
      grad_norm = .te_optional_numeric(
        d,
        "grad_norm"
      ),
      eta = .te_optional_numeric(
        d,
        "eta"
      ),
      param_norm = .te_optional_numeric(
        d,
        "param_norm"
      ),
      param_velocity = .te_optional_numeric(
        d,
        "param_velocity"
      ),
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
        iter = numeric(),
        tau = numeric(),
        loss = numeric(),
        grad_norm = numeric(),
        eta = numeric(),
        param_norm = numeric(),
        param_velocity = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Existing FormalSpec summaries for article cases
#'
#' Only analyses already stored in the imported article case are included.
#' Source-only cases are not silently reclassified.
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_formalspec_summary <- function(x) {
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

    s <- te_formalspec_summary(
      case$analysis
    )

    rows[[q]] <- cbind(
      data.frame(
        case_key = x$case_keys[[i]],
        experiment = case$experiment,
        stage = case$stage,
        case_id = case$case_id,
        role = case$classification$role[[1L]],
        semantics_origin =
          "existing_article_pipeline",
        stringsAsFactors = FALSE
      ),
      s,
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
        semantics_origin = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Existing boundary events for article cases
#'
#' @param x te_article_case_set.
#' @return data.frame.
#' @export
te_article_case_set_boundaries <- function(x) {
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

    rows[[q]] <- cbind(
      data.frame(
        case_key = x$case_keys[[i]],
        experiment = case$experiment,
        stage = case$stage,
        case_id = case$case_id,
        role = case$classification$role[[1L]],
        stringsAsFactors = FALSE
      ),
      ev,
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
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Export an article-comparison evidence bundle
#'
#' @param x te_article_case_set.
#' @param path Output directory.
#' @param overwrite Replace existing directory contents.
#' @return Normalized output path.
#' @export
te_export_article_comparison_bundle <- function(
    x,
    path,
    overwrite = FALSE) {

  te_validate_article_case_set(x, error = TRUE)

  if (dir.exists(path)) {
    existing <- list.files(
      path,
      all.files = TRUE,
      no.. = TRUE
    )

    if (length(existing) && !isTRUE(overwrite)) {
      stop(
        "Output directory is not empty; use overwrite = TRUE.",
        call. = FALSE
      )
    }

    if (length(existing) && isTRUE(overwrite)) {
      unlink(
        file.path(path, existing),
        recursive = TRUE,
        force = TRUE
      )
    }
  } else {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  .te_bundle_write_csv(
    te_article_case_set_index(x),
    file.path(path, "comparison_index.csv")
  )

  .te_bundle_write_csv(
    te_article_case_set_trace_data(x),
    file.path(path, "comparison_trace_data.csv")
  )

  .te_bundle_write_csv(
    te_article_case_set_formalspec_summary(x),
    file.path(
      path,
      "existing_formalspec_summary.csv"
    )
  )

  .te_bundle_write_csv(
    te_article_case_set_boundaries(x),
    file.path(path, "boundary_events.csv")
  )

  manifest <- data.frame(
    field = c(
      "bundle_format",
      "traceexplorer_version",
      "comparison_id",
      "semantic_policy",
      "interpolation_policy"
    ),
    value = c(
      "TraceExplorer article comparison bundle v1",
      "0.8.0",
      x$comparison_id,
      paste0(
        "Reuse existing article FormalSpec semantics only; ",
        "no silent reclassification."
      ),
      "No interpolation; tau is visualization-only."
    ),
    stringsAsFactors = FALSE
  )

  .te_bundle_write_csv(
    manifest,
    file.path(path, "manifest.csv")
  )

  saveRDS(
    x,
    file.path(path, "article_case_set.rds")
  )

  files <- list.files(
    path,
    full.names = TRUE
  )

  md5 <- tools::md5sum(files)

  .te_bundle_write_csv(
    data.frame(
      file = basename(names(md5)),
      md5 = unname(md5),
      stringsAsFactors = FALSE
    ),
    file.path(path, "checksums.csv")
  )

  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}
