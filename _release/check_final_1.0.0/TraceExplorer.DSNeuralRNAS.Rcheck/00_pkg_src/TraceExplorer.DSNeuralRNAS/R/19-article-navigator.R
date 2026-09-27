#' Build a navigable article-project catalog
#'
#' Combines the RDS inventory with the E1-E5 purpose registry. The catalog is
#' descriptive and never changes source objects.
#'
#' @param project_dir Root of DSNeuralRNAS_FormalSpec_Article.
#' @param inspect Read RDS files to classify their role.
#' @return data.frame.
#' @export
te_article_catalog <- function(project_dir, inspect = TRUE) {
  inv <- te_article_inventory(
    project_dir,
    inspect = inspect
  )

  if (!nrow(inv)) {
    inv$experiment_purpose <- character(0)
    inv$display_state <- character(0)
    return(inv)
  }

  reg <- te_article_experiment_registry()
  purpose_map <- stats::setNames(
    reg$purpose,
    reg$experiment
  )

  inv$experiment_purpose <- unname(
    purpose_map[inv$experiment]
  )

  inv$display_state <- ifelse(
    inv$role == "dynamic_pipeline",
    "dynamic_pipeline",
    ifelse(
      inv$role == "source_object",
      "bridgeable_source",
      ifelse(
        inv$role == "summary_or_validation",
        "non_dynamic_evidence",
        inv$role
      )
    )
  )

  inv
}

#' Filter an article-project catalog
#'
#' @param catalog Output of te_article_catalog().
#' @param experiment Optional E1-E5 vector.
#' @param stage Optional stage vector.
#' @param role Optional role vector.
#' @param dynamic_only Keep only dynamic or bridgeable evidence.
#' @param case_pattern Optional regular-expression search on case_id.
#' @return data.frame.
#' @export
te_article_filter <- function(
    catalog,
    experiment = NULL,
    stage = NULL,
    role = NULL,
    dynamic_only = FALSE,
    case_pattern = NULL) {

  if (!is.data.frame(catalog)) {
    stop("catalog must be a data.frame.", call. = FALSE)
  }

  required <- c(
    "experiment",
    "stage",
    "case_id",
    "role",
    "dynamic_eligible",
    "path"
  )
  missing <- setdiff(required, names(catalog))
  if (length(missing)) {
    stop(
      "Catalog is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  keep <- rep(TRUE, nrow(catalog))

  if (!is.null(experiment)) {
    keep <- keep & catalog$experiment %in% experiment
  }
  if (!is.null(stage)) {
    keep <- keep & catalog$stage %in% stage
  }
  if (!is.null(role)) {
    keep <- keep & catalog$role %in% role
  }
  if (isTRUE(dynamic_only)) {
    keep <- keep & (
      catalog$dynamic_eligible |
        catalog$bridge_eligible
    )
  }
  if (!is.null(case_pattern) && nzchar(case_pattern)) {
    keep <- keep & grepl(
      case_pattern,
      catalog$case_id,
      ignore.case = TRUE
    )
  }

  out <- catalog[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Summarize article evidence by experiment and role
#'
#' @param catalog Output of te_article_catalog().
#' @return data.frame.
#' @export
te_article_catalog_summary <- function(catalog) {
  if (!is.data.frame(catalog)) {
    stop("catalog must be a data.frame.", call. = FALSE)
  }

  if (!nrow(catalog)) {
    return(
      data.frame(
        experiment = character(),
        role = character(),
        n_objects = integer(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- stats::aggregate(
    rep(1L, nrow(catalog)),
    by = list(
      experiment = catalog$experiment,
      role = catalog$role
    ),
    FUN = sum
  )
  names(out)[[3L]] <- "n_objects"

  out <- out[
    order(out$experiment, out$role),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

#' Load one case from a catalog
#'
#' When more than one RDS maps to the same case_id, role preference is
#' explicit. By default an existing article pipeline is preferred to a
#' source-only object, and a source object is preferred to a summary.
#'
#' @param catalog Output of te_article_catalog().
#' @param case_id Exact case identifier.
#' @param experiment Optional experiment restriction.
#' @param stage Optional stage restriction.
#' @param prefer_role Ordered role preference.
#' @return te_article_case.
#' @export
te_article_load_catalog_case <- function(
    catalog,
    case_id,
    experiment = NULL,
    stage = NULL,
    prefer_role = c(
      "dynamic_pipeline",
      "source_object",
      "summary_or_validation"
    )) {

  if (length(case_id) != 1L || !nzchar(case_id)) {
    stop(
      "case_id must be one non-empty value.",
      call. = FALSE
    )
  }

  rows <- te_article_filter(
    catalog,
    experiment = experiment,
    stage = stage
  )
  rows <- rows[
    rows$case_id == case_id,
    ,
    drop = FALSE
  ]

  if (!nrow(rows)) {
    stop(
      "No catalog object matches case_id: ",
      case_id,
      ".",
      call. = FALSE
    )
  }

  priority <- match(rows$role, prefer_role)
  priority[is.na(priority)] <- length(prefer_role) + 1L

  rows <- rows[
    order(priority, rows$path),
    ,
    drop = FALSE
  ]

  te_article_load_rds(rows$path[[1L]])
}

#' Convert selected dynamic article cases into a multi-run collection
#'
#' This helper compares traces only. Existing article FormalSpec analyses remain
#' attached to their original te_article_case objects and are not pooled.
#'
#' @param cases List of te_article_case objects.
#' @param labels Optional display labels.
#' @return te_multirun.
#' @export
te_article_cases_multirun <- function(cases, labels = NULL) {
  if (!is.list(cases) || !length(cases)) {
    stop(
      "cases must be a non-empty list.",
      call. = FALSE
    )
  }

  ok <- vapply(
    cases,
    inherits,
    logical(1),
    what = "te_article_case"
  )
  if (!all(ok)) {
    stop(
      "Every case must inherit te_article_case.",
      call. = FALSE
    )
  }

  has_trace <- vapply(
    cases,
    function(x) inherits(x$trace, "te_trace"),
    logical(1)
  )
  if (!all(has_trace)) {
    stop(
      "Every selected article case must contain a dynamic trace.",
      call. = FALSE
    )
  }

  ids <- vapply(
    cases,
    function(x) {
      paste(
        .te_article_scalar(x$experiment, "EX"),
        .te_article_scalar(x$case_id, "case"),
        sep = "::"
      )
    },
    character(1)
  )

  if (anyDuplicated(ids)) {
    ids <- make.unique(ids, sep = "::")
  }

  if (is.null(labels)) {
    labels <- vapply(
      cases,
      function(x) {
        paste0(
          .te_article_scalar(x$experiment, "EX"),
          " \u00b7 ",
          .te_article_scalar(x$case_id, "case")
        )
      },
      character(1)
    )
  }

  te_multirun(
    traces = lapply(cases, function(x) x$trace),
    run_ids = ids,
    labels = labels
  )
}
