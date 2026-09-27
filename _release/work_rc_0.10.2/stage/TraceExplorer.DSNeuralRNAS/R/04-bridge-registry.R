#' Register a TraceExplorer bridge
#'
#' @param source_class Source S3 class.
#' @param extractor Function(object, ...) returning te_trace or canonical data.frame.
#' @param source_package Informational producer package.
#' @param description Human-readable description.
#' @param normative Whether the bridge is normative. Default FALSE.
#' @export
te_register_bridge <- function(source_class,
                               extractor,
                               source_package = NA_character_,
                               description = "",
                               normative = FALSE) {
  stopifnot(
    is.character(source_class),
    length(source_class) == 1L,
    is.function(extractor)
  )

  .te_env$bridges[[source_class]] <- list(
    source_class = source_class,
    extractor = extractor,
    source_package = source_package,
    description = description,
    normative = isTRUE(normative)
  )
  invisible(source_class)
}

#' Inspect registered bridges
#' @export
te_bridge_registry <- function() {
  if (length(.te_env$bridges) == 0L) {
    return(data.frame(
      source_class = character(),
      source_package = character(),
      description = character(),
      normative = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(
    rbind,
    lapply(.te_env$bridges, function(x) {
      data.frame(
        source_class = x$source_class,
        source_package = x$source_package,
        description = x$description,
        normative = x$normative,
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Detect a source object
#' @param object Any R object.
#' @export
te_detect_source <- function(object) {
  cls <- class(object)
  registered <- intersect(cls, names(.te_env$bridges))
  data.frame(
    detected_class = if (length(cls)) cls[[1L]] else typeof(object),
    registered_bridge = if (length(registered)) registered[[1L]] else NA_character_,
    has_registered_bridge = length(registered) > 0L,
    is_data_frame = is.data.frame(object),
    stringsAsFactors = FALSE
  )
}

#' Convert an object through a registered bridge
#' @param object Source object.
#' @param ... Passed to bridge extractor.
#' @export
te_bridge <- function(object, ...) {
  if (inherits(object, "te_trace")) return(object)

  cls <- class(object)
  hit <- intersect(cls, names(.te_env$bridges))
  if (length(hit) == 0L) {
    stop(
      "No TraceExplorer bridge is registered for class(es): ",
      paste(cls, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  spec <- .te_env$bridges[[hit[[1L]]]]
  ans <- spec$extractor(object, ...)

  if (inherits(ans, "te_trace")) return(ans)
  if (is.data.frame(ans)) {
    return(
      te_trace(
        ans,
        provenance = list(
          source_class = hit[[1L]],
          source_package = spec$source_package,
          adapter_mode = "registered_bridge",
          normative = spec$normative
        )
      )
    )
  }

  stop(
    "Bridge extractor must return te_trace or data.frame.",
    call. = FALSE
  )
}
