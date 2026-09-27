.te_env <- new.env(parent = emptyenv())
.te_env$bridges <- list()

.te_require_namespace <- function(pkg, feature = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0(
      "Package '", pkg, "' is required",
      if (!is.null(feature)) paste0(" for ", feature) else "",
      "."
    )
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

.te_first_existing <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0L) NULL else hit[[1L]]
}

.te_as_numeric <- function(x, name) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) != length(x) || any(!is.finite(y))) {
    stop("Field '", name, "' must be finite numeric.", call. = FALSE)
  }
  y
}

.te_safe_log_ratio <- function(x, ref = NULL) {
  x <- as.numeric(x)
  if (is.null(ref)) ref <- x[[1L]]
  eps <- .Machine$double.eps
  log(pmax(x, eps) / max(ref, eps))
}

.te_norm_rows <- function(m) {
  m <- as.matrix(m)
  storage.mode(m) <- "double"
  sqrt(rowSums(m^2))
}

.te_parameter_columns <- function(df) {
  grep(
    "^(theta|w|weight|b|bias)[._]?[0-9]+$",
    names(df),
    value = TRUE,
    ignore.case = TRUE
  )
}
