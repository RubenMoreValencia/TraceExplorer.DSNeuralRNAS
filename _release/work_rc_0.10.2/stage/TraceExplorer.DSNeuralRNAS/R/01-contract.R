#' TraceExplorer observable contract
#'
#' @return A data.frame describing required, recommended and optional fields.
#' @export
te_contract <- function() {
  data.frame(
    field = c(
      "iter", "loss", "grad_norm", "eta",
      "param_norm", "param_velocity",
      "delta_loss", "time",
      "experiment_id", "case_id", "source_package",
      "source_class", "adapter_mode"
    ),
    level = c(
      "required", "required", "recommended", "recommended",
      "recommended", "recommended",
      "derived", "optional",
      rep("provenance", 5)
    ),
    mathematical_role = c(
      "k",
      "L_k",
      "||grad L_k||_2",
      "eta_k",
      "||theta_k||_2",
      "||theta_k-theta_{k-1}||_2",
      "Delta L_k",
      "t_k",
      rep(NA_character_, 5)
    ),
    didactic_meaning = c(
      "Orden del aprendizaje",
      "Desajuste o tension del aprendizaje",
      "Presion local de cambio",
      "Intensidad de actualizacion",
      "Magnitud global del estado parametrico",
      "Movimiento interno entre actualizaciones",
      "Cambio de perdida entre iteraciones",
      "Tiempo continuo o fisico, cuando exista",
      "Experimento de procedencia",
      "Caso o escenario",
      "Paquete productor",
      "Clase del objeto fuente",
      "Modo de conversion al contrato"
    ),
    stringsAsFactors = FALSE
  )
}

#' Validate a canonical trace data frame
#'
#' @param x data.frame.
#' @param error Whether to stop on invalid input.
#' @return A validation table.
#' @export
te_validate_trace <- function(x, error = TRUE) {
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

  add("is_data_frame", is.data.frame(x), "Input must be a data.frame.")
  if (!is.data.frame(x)) {
    if (isTRUE(error)) stop("Trace must be a data.frame.", call. = FALSE)
    return(checks)
  }

  add("has_iter", "iter" %in% names(x), "Required field: iter.")
  add("has_loss", "loss" %in% names(x), "Required field: loss.")

  if ("iter" %in% names(x)) {
    iter <- suppressWarnings(as.numeric(x$iter))
    add(
      "iter_finite",
      all(is.finite(iter)),
      "iter must be finite numeric."
    )
    add(
      "iter_increasing",
      length(iter) <= 1L || all(diff(iter) > 0),
      "iter must be strictly increasing."
    )
  }

  if ("loss" %in% names(x)) {
    loss <- suppressWarnings(as.numeric(x$loss))
    add(
      "loss_finite",
      all(is.finite(loss)),
      "loss must be finite numeric."
    )
  }

  numeric_optional <- intersect(
    c(
      "grad_norm", "eta", "param_norm", "param_velocity",
      "delta_loss", "time"
    ),
    names(x)
  )
  for (nm in numeric_optional) {
    xx <- suppressWarnings(as.numeric(x[[nm]]))
    ok <- all(is.finite(xx) | is.na(xx))
    add(
      paste0(nm, "_numeric"),
      ok,
      paste0(nm, " must be numeric or NA.")
    )
  }

  add(
    "minimum_rows",
    nrow(x) >= 2L,
    "At least two trace rows are required for trajectory exploration."
  )

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "TraceExplorer contract validation failed:",
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
