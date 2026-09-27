#' Registry of first-article experiment purposes
#'
#' @return data.frame.
#' @export
te_article_experiment_registry <- function() {
  data.frame(
    experiment = paste0("E", 1:5),
    purpose = c(
      "Analytical quadratic reference",
      "Multiseed and controlled theta0 perturbation robustness",
      "Positive affine loss equivariance and shift invariance",
      "Same endpoint, different learning trajectory",
      "Real DSNeuralRNAS applicability"
    ),
    stringsAsFactors = FALSE
  )
}

.te_article_has_pipeline_fields <- function(x) {
  is.list(x) &&
    all(c(
      "trace",
      "windows",
      "candidates",
      "confirmed",
      "boundaries"
    ) %in% names(x))
}

.te_article_extract_pipeline <- function(object) {
  if (inherits(object, "te_formalspec_analysis")) {
    return(object)
  }

  if (.te_article_has_pipeline_fields(object)) {
    return(object)
  }

  if (
    is.list(object) &&
    "pipeline" %in% names(object) &&
    .te_article_has_pipeline_fields(object$pipeline)
  ) {
    return(object$pipeline)
  }

  NULL
}

.te_article_scalar <- function(x, fallback = NA_character_) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(fallback)
  }
  y <- as.character(x[[1L]])
  if (!nzchar(y)) fallback else y
}

.te_article_infer_ids <- function(
    source_path = NULL,
    source_name = NULL,
    object = NULL,
    pipeline = NULL) {

  name <- if (!is.null(source_name)) {
    basename(source_name)
  } else if (!is.null(source_path)) {
    basename(source_path)
  } else {
    "in_memory_object"
  }

  stem <- sub("\\.rds$", "", name, ignore.case = TRUE)

  experiment <- NA_character_
  stage <- NA_character_
  case_id <- stem

  m_exp <- regexpr("E[1-5]", stem, perl = TRUE)
  if (m_exp[[1L]] > 0L) {
    experiment <- regmatches(stem, m_exp)
  }

  m_stage <- regexpr("E[1-5][A-Z]", stem, perl = TRUE)
  if (m_stage[[1L]] > 0L) {
    stage <- regmatches(stem, m_stage)
  }

  if (!is.null(source_path)) {
    parts <- strsplit(
      normalizePath(
        source_path,
        winslash = "/",
        mustWork = FALSE
      ),
      "/",
      fixed = TRUE
    )[[1L]]

    exp_part <- parts[grepl("^E[1-5]$", parts)]
    if (length(exp_part)) {
      experiment <- exp_part[[length(exp_part)]]
    }

    stage_part <- parts[grepl("^E[1-5][A-Z]$", parts)]
    if (length(stage_part)) {
      stage <- stage_part[[length(stage_part)]]
    }
  }

  metadata_candidates <- list(
    if (!is.null(pipeline)) pipeline$metadata else NULL,
    if (!is.null(pipeline) &&
        is.list(pipeline$trace)) pipeline$trace$metadata else NULL,
    if (is.list(object)) object$metadata else NULL
  )

  for (md in metadata_candidates) {
    if (
      is.list(md) &&
      !is.null(md$experiment_id) &&
      length(md$experiment_id)
    ) {
      case_id <- as.character(md$experiment_id[[1L]])
      break
    }
  }

  if (
    is.list(object) &&
    "summary" %in% names(object) &&
    is.data.frame(object$summary) &&
    nrow(object$summary) >= 1L
  ) {
    for (nm in c("scenario_id", "experiment_id", "case_id")) {
      if (nm %in% names(object$summary)) {
        case_id <- as.character(object$summary[[nm]][[1L]])
        break
      }
    }
  }

  case_id <- sub("_pipeline$", "", case_id)
  case_id <- sub("_source$", "", case_id)

  data.frame(
    experiment = experiment,
    stage = stage,
    case_id = case_id,
    source_name = name,
    stringsAsFactors = FALSE
  )
}

#' Classify a first-article R object
#'
#' @param object R object from the article project.
#' @return One-row data.frame.
#' @export
te_article_classify_object <- function(object) {
  pipeline <- .te_article_extract_pipeline(object)

  if (!is.null(pipeline)) {
    return(
      data.frame(
        role = "dynamic_pipeline",
        dynamic_eligible = TRUE,
        bridge_eligible = FALSE,
        object_class = paste(class(object), collapse = " / "),
        stringsAsFactors = FALSE
      )
    )
  }

  detected <- tryCatch(
    te_detect_source(object),
    error = function(e) NULL
  )

  bridge_ok <- !is.null(detected) &&
    isTRUE(detected$has_registered_bridge[[1L]])

  if (bridge_ok) {
    return(
      data.frame(
        role = "source_object",
        dynamic_eligible = TRUE,
        bridge_eligible = TRUE,
        object_class = paste(class(object), collapse = " / "),
        stringsAsFactors = FALSE
      )
    )
  }

  data.frame(
    role = "summary_or_validation",
    dynamic_eligible = FALSE,
    bridge_eligible = FALSE,
    object_class = paste(class(object), collapse = " / "),
    stringsAsFactors = FALSE
  )
}

.te_article_analysis_from_pipeline <- function(pipeline) {
  required <- c(
    "trace",
    "observables",
    "windows",
    "candidates",
    "confirmed",
    "boundaries"
  )

  if (!all(required %in% names(pipeline))) {
    return(NULL)
  }

  if (inherits(pipeline, "te_formalspec_analysis")) {
    return(pipeline)
  }

  out <- structure(
    list(
      trace = pipeline$trace,
      observables = pipeline$observables,
      windows = pipeline$windows,
      candidates = pipeline$candidates,
      confirmed = pipeline$confirmed,
      boundaries = pipeline$boundaries,
      protocol = te_formalspec_protocol(),
      calibration_on_trace = FALSE,
      freeze_id = "DSFS-1.0.0-FIRST-ARTICLE",
      imported_existing_semantics = TRUE
    ),
    class = "te_formalspec_analysis"
  )

  te_validate_formalspec_analysis(out, error = TRUE)
  out
}

.te_article_trace_from_pipeline <- function(
    pipeline,
    ids,
    source_path = NULL,
    source_md5 = NA_character_) {

  d <- DSNeuralRNAS.FormalSpec::dsfs_trace_data(
    pipeline$trace
  )

  p <- list(
    experiment_id = ids$case_id[[1L]],
    source_package = "DSNeuralRNAS_FormalSpec_Article",
    source_package_version = "v7.1-compatible",
    source_class = "article_formalspec_pipeline",
    adapter_mode = "existing_article_evidence",
    dataset_id = .te_article_scalar(
      if (is.list(pipeline$trace)) {
        pipeline$trace$metadata$dataset_id
      } else {
        NULL
      },
      "article-defined"
    ),
    model_id = .te_article_scalar(
      if (is.list(pipeline$trace)) {
        pipeline$trace$metadata$model_id
      } else {
        NULL
      },
      "article-defined"
    ),
    optimizer = .te_article_scalar(
      if (is.list(pipeline$trace)) {
        pipeline$trace$metadata$optimizer
      } else {
        NULL
      },
      "article-defined"
    ),
    article_experiment = ids$experiment[[1L]],
    article_stage = ids$stage[[1L]],
    article_case_id = ids$case_id[[1L]],
    article_source_path = source_path,
    article_source_md5 = source_md5,
    semantics_origin = "existing_article_pipeline",
    recomputed_formalspec = FALSE,
    legacy_semantics_promoted = FALSE
  )

  te_trace(
    d,
    provenance = p
  )
}

#' Import an in-memory first-article object
#'
#' Dynamic FormalSpec pipelines are reused as existing evidence. Registered
#' source objects are bridged without silently manufacturing missing history.
#'
#' @param object Article project object.
#' @param source_path Optional original RDS path.
#' @param source_name Optional display filename.
#' @return te_article_case.
#' @export
te_article_import_object <- function(
    object,
    source_path = NULL,
    source_name = NULL) {

  classification <- te_article_classify_object(object)
  pipeline <- .te_article_extract_pipeline(object)

  source_md5 <- if (
    !is.null(source_path) &&
    file.exists(source_path)
  ) {
    unname(tools::md5sum(source_path)[[1L]])
  } else {
    NA_character_
  }

  ids <- .te_article_infer_ids(
    source_path = source_path,
    source_name = source_name,
    object = object,
    pipeline = pipeline
  )

  if (!is.null(pipeline)) {
    tr <- .te_article_trace_from_pipeline(
      pipeline,
      ids = ids,
      source_path = source_path,
      source_md5 = source_md5
    )

    analysis <- .te_article_analysis_from_pipeline(
      pipeline
    )

    out <- structure(
      list(
        trace = tr,
        analysis = analysis,
        source_object = object,
        classification = classification,
        experiment = ids$experiment[[1L]],
        stage = ids$stage[[1L]],
        case_id = ids$case_id[[1L]],
        source_name = ids$source_name[[1L]],
        source_path = source_path,
        source_md5 = source_md5,
        uses_existing_article_semantics = !is.null(analysis)
      ),
      class = "te_article_case"
    )

    return(out)
  }

  if (isTRUE(classification$bridge_eligible[[1L]])) {
    tr <- te_bridge(object)
    tr$provenance$article_experiment <-
      ids$experiment[[1L]]
    tr$provenance$article_stage <-
      ids$stage[[1L]]
    tr$provenance$article_case_id <-
      ids$case_id[[1L]]
    tr$provenance$article_source_path <-
      source_path
    tr$provenance$article_source_md5 <-
      source_md5
    tr$provenance$semantics_origin <-
      "article_source_object_bridge"
    tr$provenance$recomputed_formalspec <- FALSE

    return(
      structure(
        list(
          trace = tr,
          analysis = NULL,
          source_object = object,
          classification = classification,
          experiment = ids$experiment[[1L]],
          stage = ids$stage[[1L]],
          case_id = ids$case_id[[1L]],
          source_name = ids$source_name[[1L]],
          source_path = source_path,
          source_md5 = source_md5,
          uses_existing_article_semantics = FALSE
        ),
        class = "te_article_case"
      )
    )
  }

  structure(
    list(
      trace = NULL,
      analysis = NULL,
      source_object = object,
      classification = classification,
      experiment = ids$experiment[[1L]],
      stage = ids$stage[[1L]],
      case_id = ids$case_id[[1L]],
      source_name = ids$source_name[[1L]],
      source_path = source_path,
      source_md5 = source_md5,
      uses_existing_article_semantics = FALSE
    ),
    class = "te_article_case"
  )
}

#' Load a first-article RDS object
#'
#' @param path RDS path.
#' @param source_name Optional original filename when path is temporary.
#' @return te_article_case.
#' @export
te_article_load_rds <- function(path, source_name = NULL) {
  if (!file.exists(path)) {
    stop("Article RDS file does not exist.", call. = FALSE)
  }

  object <- readRDS(path)

  te_article_import_object(
    object,
    source_path = normalizePath(
      path,
      winslash = "/",
      mustWork = TRUE
    ),
    source_name = source_name
  )
}


#' Determine the display state of an imported article case
#'
#' This is an interface contract: non-dynamic article evidence remains
#' inspectable without being converted into a pseudo-trace.
#'
#' @param x te_article_case.
#' @return One-row data.frame.
#' @export
te_article_display_state <- function(x) {
  if (!inherits(x, "te_article_case")) {
    stop("Expected te_article_case.", call. = FALSE)
  }

  has_trace <- inherits(x$trace, "te_trace")
  has_analysis <- inherits(
    x$analysis,
    "te_formalspec_analysis"
  )
  bridgeable <- isTRUE(
    x$classification$bridge_eligible[[1L]]
  )

  state <- if (has_analysis) {
    "dynamic_pipeline"
  } else if (has_trace && bridgeable) {
    "bridgeable_source"
  } else if (has_trace) {
    "dynamic_trace"
  } else {
    "non_dynamic_evidence"
  }

  data.frame(
    state = state,
    has_trace = has_trace,
    has_formalspec_analysis = has_analysis,
    trace_views_enabled = has_trace,
    formalspec_views_enabled =
      has_analysis || has_trace,
    role = x$classification$role[[1L]],
    message = switch(
      state,
      dynamic_pipeline = paste0(
        "Pipeline din\u00e1mico: se reutiliza la sem\u00e1ntica FormalSpec ",
        "existente del art\u00edculo."
      ),
      bridgeable_source = paste0(
        "Objeto fuente bridgeable: existe traza can\u00f3nica, pero no se ",
        "atribuye sem\u00e1ntica FormalSpec del art\u00edculo salvo que exista ",
        "expl\u00edcitamente."
      ),
      dynamic_trace = "Traza din\u00e1mica disponible.",
      non_dynamic_evidence = paste0(
        "Evidencia no din\u00e1mica: puede inspeccionarse como control, ",
        "resumen o validaci\u00f3n; las vistas de trayectoria permanecen ",
        "deshabilitadas."
      )
    ),
    stringsAsFactors = FALSE
  )
}


#' Summarize an imported article case
#'
#' @param x te_article_case.
#' @return One-row data.frame.
#' @export
te_article_case_summary <- function(x) {
  if (!inherits(x, "te_article_case")) {
    stop("Expected te_article_case.", call. = FALSE)
  }

  n_states <- if (inherits(x$trace, "te_trace")) {
    nrow(te_trace_data(x$trace))
  } else {
    NA_integer_
  }

  n_windows <- NA_integer_
  n_events <- NA_integer_
  candidate_regimes <- NA_integer_
  confirmed_regimes <- NA_integer_

  if (inherits(x$analysis, "te_formalspec_analysis")) {
    s <- te_formalspec_summary(x$analysis)
    n_windows <- s$n_windows[[1L]]
    n_events <- s$n_boundary_events[[1L]]
    candidate_regimes <- s$n_candidate_regimes[[1L]]
    confirmed_regimes <- s$n_confirmed_regimes[[1L]]
  }

  data.frame(
    experiment = x$experiment,
    stage = x$stage,
    case_id = x$case_id,
    role = x$classification$role[[1L]],
    dynamic_eligible =
      x$classification$dynamic_eligible[[1L]],
    bridge_eligible =
      x$classification$bridge_eligible[[1L]],
    object_class =
      x$classification$object_class[[1L]],
    n_states = n_states,
    n_windows = n_windows,
    n_candidate_regimes = candidate_regimes,
    n_confirmed_regimes = confirmed_regimes,
    n_boundary_events = n_events,
    existing_article_semantics =
      x$uses_existing_article_semantics,
    source_md5 = x$source_md5,
    source_name = x$source_name,
    stringsAsFactors = FALSE
  )
}

#' Inventory RDS evidence in a first-article project directory
#'
#' @param project_dir Root of DSNeuralRNAS_FormalSpec_Article.
#' @param inspect Read each RDS to classify its role.
#' @return data.frame.
#' @export
te_article_inventory <- function(project_dir, inspect = TRUE) {
  root <- normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  )

  object_root <- file.path(root, "objects")
  if (!dir.exists(object_root)) {
    stop(
      "The project does not contain an objects directory.",
      call. = FALSE
    )
  }

  files <- list.files(
    object_root,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (!length(files)) {
    return(
      data.frame(
        experiment = character(),
        stage = character(),
        case_id = character(),
        role = character(),
        dynamic_eligible = logical(),
        bridge_eligible = logical(),
        object_class = character(),
        source_md5 = character(),
        path = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  rows <- lapply(files, function(path) {
    rel <- substring(
      normalizePath(path, winslash = "/", mustWork = TRUE),
      nchar(root) + 2L
    )
    parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]

    experiment <- if (
      length(parts) >= 2L &&
      grepl("^E[1-5]$", parts[[2L]])
    ) {
      parts[[2L]]
    } else {
      NA_character_
    }

    stage <- if (
      length(parts) >= 3L &&
      grepl("^E[1-5][A-Z]$", parts[[3L]])
    ) {
      parts[[3L]]
    } else {
      NA_character_
    }

    stem <- sub("\\.rds$", "", basename(path))
    case_id <- sub("_(pipeline|source)$", "", stem)

    if (isTRUE(inspect)) {
      obj <- tryCatch(
        readRDS(path),
        error = function(e) e
      )

      if (inherits(obj, "error")) {
        cls <- data.frame(
          role = "read_error",
          dynamic_eligible = FALSE,
          bridge_eligible = FALSE,
          object_class = conditionMessage(obj),
          stringsAsFactors = FALSE
        )
      } else {
        cls <- te_article_classify_object(obj)
      }
    } else {
      cls <- data.frame(
        role = "not_inspected",
        dynamic_eligible = NA,
        bridge_eligible = NA,
        object_class = NA_character_,
        stringsAsFactors = FALSE
      )
    }

    data.frame(
      experiment = experiment,
      stage = stage,
      case_id = case_id,
      role = cls$role[[1L]],
      dynamic_eligible = cls$dynamic_eligible[[1L]],
      bridge_eligible = cls$bridge_eligible[[1L]],
      object_class = cls$object_class[[1L]],
      source_md5 =
        unname(tools::md5sum(path)[[1L]]),
      path = normalizePath(
        path,
        winslash = "/",
        mustWork = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$experiment, out$stage, out$case_id), , drop = FALSE]
}

#' Read the article experiment registry from a project directory
#'
#' @param project_dir Article project root.
#' @return data.frame.
#' @export
te_article_project_registry <- function(project_dir) {
  path <- file.path(
    project_dir,
    "config",
    "experiment_registry.csv"
  )

  if (!file.exists(path)) {
    stop(
      "experiment_registry.csv was not found.",
      call. = FALSE
    )
  }

  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
