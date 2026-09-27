#' Resolve browser visualization policy
#'
#' @param webgl_supported TRUE, FALSE, NULL or NA.
#' @return One-row data.frame.
#' @export
te_webgl_display_policy <- function(webgl_supported = NA) {
  if (
    is.null(webgl_supported) ||
    length(webgl_supported) != 1L ||
    is.na(webgl_supported)
  ) {
    state <- "unknown"
  } else if (isTRUE(webgl_supported)) {
    state <- "webgl"
  } else {
    state <- "fallback_2d"
  }

  data.frame(
    state = state,
    use_3d = identical(state, "webgl"),
    use_2d_fallback = identical(state, "fallback_2d"),
    message = switch(
      state,
      webgl = "WebGL disponible: visualizaci\u00f3n 3D interactiva habilitada.",
      fallback_2d = paste0(
        "WebGL no est\u00e1 disponible en este navegador. ",
        "TraceExplorer presenta una proyecci\u00f3n 2D de respaldo; ",
        "los datos y la sem\u00e1ntica permanecen intactos."
      ),
      unknown = "Detectando capacidad WebGL del navegador..."
    ),
    stringsAsFactors = FALSE
  )
}
