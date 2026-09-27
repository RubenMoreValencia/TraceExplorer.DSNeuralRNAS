.te_bundle_write_csv <- function(x, path) {
  utils::write.csv(
    x,
    path,
    row.names = FALSE,
    na = "NA"
  )
  invisible(path)
}

.te_bundle_provenance_table <- function(trace) {
  p <- trace$provenance

  if (is.null(p) || !length(p)) {
    return(
      data.frame(
        field = character(),
        value = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  vals <- vapply(
    p,
    function(x) {
      if (is.null(x)) {
        return(NA_character_)
      }
      if (is.atomic(x) && length(x) <= 8L) {
        return(paste(as.character(x), collapse = " | "))
      }
      paste0(
        "<",
        paste(class(x), collapse = "/"),
        "; length=",
        length(x),
        ">"
      )
    },
    character(1)
  )

  data.frame(
    field = names(vals),
    value = unname(vals),
    stringsAsFactors = FALSE
  )
}

#' Export an auditable TraceExplorer evidence directory
#'
#' The exporter writes derived copies only. It never modifies the source
#' article object or FormalSpec object.
#'
#' @param x te_article_case or te_trace.
#' @param path Output directory.
#' @param analysis Optional te_formalspec_analysis for a te_trace.
#' @param overwrite Replace an existing directory.
#' @return Normalized output path.
#' @export
te_export_evidence_bundle <- function(
    x,
    path,
    analysis = NULL,
    overwrite = FALSE) {

  if (inherits(x, "te_article_case")) {
    trace <- x$trace
    if (is.null(analysis)) {
      analysis <- x$analysis
    }
    article_summary <- te_article_case_summary(x)
  } else if (inherits(x, "te_trace")) {
    trace <- x
    article_summary <- NULL
  } else {
    stop(
      "x must be te_article_case or te_trace.",
      call. = FALSE
    )
  }

  if (!inherits(trace, "te_trace")) {
    stop(
      "The supplied object does not contain a dynamic trace.",
      call. = FALSE
    )
  }

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
    if (isTRUE(overwrite) && length(existing)) {
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

  trace_path <- file.path(path, "canonical_trace.csv")
  .te_bundle_write_csv(
    te_trace_data(trace),
    trace_path
  )

  .te_bundle_write_csv(
    .te_bundle_provenance_table(trace),
    file.path(path, "provenance.csv")
  )

  .te_bundle_write_csv(
    te_capabilities(trace),
    file.path(path, "capabilities.csv")
  )

  .te_bundle_write_csv(
    te_space_availability(trace),
    file.path(path, "space_availability.csv")
  )

  if (inherits(trace$architecture, "te_architecture")) {
    .te_bundle_write_csv(
      te_architecture_data(trace$architecture),
      file.path(path, "architecture.csv")
    )
  }

  if (inherits(trace$parameter_map, "te_parameter_map")) {
    .te_bundle_write_csv(
      te_parameter_map_data(trace$parameter_map),
      file.path(path, "parameter_map.csv")
    )
  }

  if (!is.null(article_summary)) {
    .te_bundle_write_csv(
      article_summary,
      file.path(path, "article_case_summary.csv")
    )
  }

  if (inherits(analysis, "te_formalspec_analysis")) {
    te_validate_formalspec_analysis(
      analysis,
      error = TRUE
    )

    .te_bundle_write_csv(
      DSNeuralRNAS.FormalSpec::dsfs_windows_data(
        analysis$windows
      ),
      file.path(path, "windows.csv")
    )
    .te_bundle_write_csv(
      DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
        analysis$candidates
      ),
      file.path(path, "candidate_regimes.csv")
    )
    .te_bundle_write_csv(
      DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
        analysis$confirmed
      ),
      file.path(path, "confirmed_regimes.csv")
    )
    .te_bundle_write_csv(
      DSNeuralRNAS.FormalSpec::dsfs_boundary_events_data(
        analysis$boundaries
      ),
      file.path(path, "boundary_events.csv")
    )
    .te_bundle_write_csv(
      te_formalspec_summary(analysis),
      file.path(path, "formalspec_summary.csv")
    )
  }

  manifest <- data.frame(
    field = c(
      "bundle_format",
      "traceexplorer_version",
      "formal_spec_freeze",
      "source_semantics_recomputed",
      "export_note"
    ),
    value = c(
      "TraceExplorer evidence bundle v1",
      "0.6.0",
      "DSFS-1.0.0-FIRST-ARTICLE",
      "FALSE",
      paste0(
        "Derived evidence copy; source objects are not modified."
      )
    ),
    stringsAsFactors = FALSE
  )

  .te_bundle_write_csv(
    manifest,
    file.path(path, "manifest.csv")
  )

  saveRDS(
    list(
      trace = trace,
      analysis = analysis,
      article_summary = article_summary
    ),
    file.path(path, "traceexplorer_bundle.rds")
  )

  readme <- c(
    "TraceExplorer.DSNeuralRNAS evidence bundle",
    "",
    "This directory is a derived, auditable representation.",
    "It does not modify or replace the original producer object.",
    "",
    "canonical_trace.csv: canonical observable trajectory",
    "provenance.csv: source/bridge provenance",
    "capabilities.csv: available observability depth",
    "space_availability.csv: valid 3D spaces",
    "architecture.csv: optional architecture contract",
    "parameter_map.csv: optional exact parameter history",
    "windows/candidate/confirmed/boundary files: optional FormalSpec evidence",
    "traceexplorer_bundle.rds: serialized bundle for R",
    "checksums.csv: MD5 of exported files"
  )
  writeLines(
    readme,
    con = file.path(path, "README.txt"),
    useBytes = TRUE
  )

  files <- list.files(
    path,
    full.names = TRUE
  )
  files <- files[
    basename(files) != "checksums.csv"
  ]

  md5 <- tools::md5sum(files)

  checksums <- data.frame(
    file = basename(names(md5)),
    md5 = unname(md5),
    stringsAsFactors = FALSE
  )
  checksums <- checksums[
    order(checksums$file),
    ,
    drop = FALSE
  ]

  .te_bundle_write_csv(
    checksums,
    file.path(path, "checksums.csv")
  )

  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}
