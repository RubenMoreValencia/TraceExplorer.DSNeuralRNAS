#' Define a generic neural architecture contract
#'
#' @param layers data.frame with one row per trainable or observable block.
#' Required fields: layer_id, layer_index, layer_type, input_dim, output_dim,
#' activation, trainable.
#' @param model_id Stable model identifier.
#' @param family Model family label.
#' @return Object of class te_architecture.
#' @export
te_architecture <- function(
    layers,
    model_id = "unnamed-model",
    family = "mlp") {

  req <- c(
    "layer_id",
    "layer_index",
    "layer_type",
    "input_dim",
    "output_dim",
    "activation",
    "trainable"
  )

  if (!is.data.frame(layers)) {
    stop("layers must be a data.frame.", call. = FALSE)
  }

  missing <- setdiff(req, names(layers))
  if (length(missing)) {
    stop(
      "Architecture contract is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  x <- layers[, req, drop = FALSE]
  x$layer_id <- as.character(x$layer_id)
  x$layer_index <- as.integer(x$layer_index)
  x$layer_type <- as.character(x$layer_type)
  x$input_dim <- as.integer(x$input_dim)
  x$output_dim <- as.integer(x$output_dim)
  x$activation <- as.character(x$activation)
  x$trainable <- as.logical(x$trainable)

  if (anyDuplicated(x$layer_id)) {
    stop("layer_id values must be unique.", call. = FALSE)
  }
  if (any(!is.finite(x$layer_index)) ||
      any(x$layer_index < 1L)) {
    stop("layer_index must be positive integer.", call. = FALSE)
  }
  if (any(!is.finite(x$input_dim)) ||
      any(!is.finite(x$output_dim)) ||
      any(x$input_dim < 1L) ||
      any(x$output_dim < 1L)) {
    stop(
      "input_dim and output_dim must be positive integer.",
      call. = FALSE
    )
  }

  x$n_weight_parameters <- x$input_dim * x$output_dim
  x$n_bias_parameters <- ifelse(x$trainable, x$output_dim, 0L)
  x$n_parameters <- x$n_weight_parameters + x$n_bias_parameters

  structure(
    list(
      layers = x,
      model_id = as.character(model_id),
      family = as.character(family),
      n_layers = nrow(x),
      n_parameters = sum(x$n_parameters)
    ),
    class = "te_architecture"
  )
}

#' Extract architecture table
#'
#' @param x te_architecture.
#' @return data.frame.
#' @export
te_architecture_data <- function(x) {
  if (!inherits(x, "te_architecture")) {
    stop("Expected te_architecture.", call. = FALSE)
  }
  x$layers
}

#' Validate architecture contract
#'
#' @param x te_architecture.
#' @param error Stop on failure.
#' @return validation table.
#' @export
te_validate_architecture <- function(x, error = FALSE) {
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

  add(
    "class",
    inherits(x, "te_architecture"),
    "Object must inherit te_architecture."
  )

  if (inherits(x, "te_architecture")) {
    d <- x$layers
    add(
      "layer_ids_unique",
      !anyDuplicated(d$layer_id),
      "Layer identifiers must be unique."
    )
    add(
      "dimensions_positive",
      all(d$input_dim > 0L & d$output_dim > 0L),
      "Layer dimensions must be positive."
    )
    add(
      "parameter_count",
      identical(
        as.numeric(x$n_parameters),
        as.numeric(sum(d$n_parameters))
      ),
      "Total parameter count must equal the layer sum."
    )
  }

  if (isTRUE(error) && any(checks$status == "FAIL")) {
    stop(
      paste(
        "Architecture validation failed:",
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

#' Architecture contract for a dense multilayer perceptron
#'
#' @param dims Integer vector c(input, hidden..., output).
#' @param activations Activation per trainable layer.
#' @param model_id Stable identifier.
#' @return te_architecture.
#' @export
te_mlp_architecture <- function(
    dims,
    activations = NULL,
    model_id = "generic-mlp") {

  dims <- as.integer(dims)

  if (length(dims) < 2L || any(dims < 1L)) {
    stop(
      "dims must contain at least input and output dimensions.",
      call. = FALSE
    )
  }

  L <- length(dims) - 1L

  if (is.null(activations)) {
    activations <- c(
      rep("tanh", max(L - 1L, 0L)),
      "linear"
    )
  }

  if (length(activations) != L) {
    stop(
      "activations must have one value per trainable layer.",
      call. = FALSE
    )
  }

  layers <- data.frame(
    layer_id = paste0("layer_", seq_len(L)),
    layer_index = seq_len(L),
    layer_type = rep("dense", L),
    input_dim = dims[-length(dims)],
    output_dim = dims[-1L],
    activation = as.character(activations),
    trainable = rep(TRUE, L),
    stringsAsFactors = FALSE
  )

  te_architecture(
    layers = layers,
    model_id = model_id,
    family = "mlp"
  )
}
