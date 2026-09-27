.te_pkg_version_or_unknown <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return("not-installed")
  }
  as.character(utils::packageVersion(pkg))
}

#' Inspect TraceExplorer observability capabilities
#'
#' Capability reflects what the source actually recorded. Missing evidence is
#' not reconstructed by assumption.
#'
#' @param trace te_trace.
#' @return One-row data.frame.
#' @export
te_capabilities <- function(trace) {
  if (!inherits(trace, "te_trace")) {
    stop("Expected te_trace.", call. = FALSE)
  }

  d <- te_trace_data(trace)
  has_map <- inherits(trace$parameter_map, "te_parameter_map")

  has_loss <- "loss" %in% names(d)
  has_grad <- "grad_norm" %in% names(d) &&
    any(!is.na(d$grad_norm))
  has_eta <- "eta" %in% names(d) &&
    any(!is.na(d$eta))
  has_param_norm <- "param_norm" %in% names(d) &&
    any(!is.na(d$param_norm))
  has_velocity <- "param_velocity" %in% names(d) &&
    any(!is.na(d$param_velocity))

  level <- if (has_map) {
    "parameter_history"
  } else if (has_param_norm && has_velocity) {
    "parametric_dynamics"
  } else if (has_grad || has_eta || has_param_norm) {
    "global_dynamics"
  } else {
    "minimal_trace"
  }

  data.frame(
    capability = level,
    loss = has_loss,
    gradient_norm = has_grad,
    learning_rate = has_eta,
    parameter_norm = has_param_norm,
    parameter_velocity = has_velocity,
    parameter_history = has_map,
    architecture_contract =
      inherits(trace$architecture, "te_architecture"),
    formalspec_structurally_eligible =
      has_loss && nrow(d) >= 3L,
    stringsAsFactors = FALSE
  )
}

#' Bridge a real DSNeuralRNAS MLP training object
#'
#' `rnas_mlp_train` records loss, gradient norm and block norms. Global
#' parameter norm is exactly reconstructed from those block norms. Parameter
#' velocity is deliberately not invented because the source does not retain
#' full parameter history.
#'
#' @param object rnas_mlp_train.
#' @return te_trace.
#' @export
te_bridge_rnas_mlp <- function(object) {
  if (!inherits(object, "rnas_mlp_train")) {
    stop("Expected rnas_mlp_train.", call. = FALSE)
  }

  tray <- as.data.frame(object$trayectoria)
  req <- c(
    "iter", "loss", "grad_norm",
    "W_norm", "b1_norm", "v_norm", "b2"
  )
  missing <- setdiff(req, names(tray))
  if (length(missing)) {
    stop(
      "rnas_mlp_train trajectory is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  eta <- object$configuracion$eta
  param_norm <- sqrt(
    tray$W_norm^2 +
      tray$b1_norm^2 +
      tray$v_norm^2 +
      tray$b2^2
  )

  d <- data.frame(
    iter = tray$iter,
    loss = tray$loss,
    grad_norm = tray$grad_norm,
    eta = rep(as.numeric(eta), nrow(tray)),
    param_norm = param_norm,
    source_W_norm = tray$W_norm,
    source_b1_norm = tray$b1_norm,
    source_v_norm = tray$v_norm,
    source_b2 = tray$b2,
    stringsAsFactors = FALSE
  )

  te_trace(
    d,
    provenance = list(
      experiment_id = "rnas-mlp-runtime",
      source_package = "DSNeuralRNAS",
      source_package_version =
        .te_pkg_version_or_unknown("DSNeuralRNAS"),
      source_class = "rnas_mlp_train",
      adapter_mode = "explicit_rnas_mlp_bridge",
      dataset_id = "runtime-matrix",
      model_id = paste0(
        "mlp-",
        object$configuracion$d_input,
        "-",
        object$configuracion$d_hidden,
        "-1"
      ),
      optimizer = "gradient_descent",
      seed = object$configuracion$seed,
      architecture = list(
        d_input = object$configuracion$d_input,
        d_hidden = object$configuracion$d_hidden,
        d_output = 1L,
        activation = object$configuracion$activation
      ),
      parameter_velocity_status =
        "unavailable_source_no_parameter_history",
      legacy_semantics_promoted = FALSE
    )
  )
}

#' Build an exact parameter map from ML.DSNeuralRNAS theta history
#'
#' The ordering follows the producer's own pack function:
#' as.vector(W1), b1, W2, b2.
#'
#' @param object ml_variable_aprendida.
#' @return te_parameter_map.
#' @export
te_parameter_map_ml_variable <- function(object) {
  if (!inherits(object, "ml_variable_aprendida")) {
    stop("Expected ml_variable_aprendida.", call. = FALSE)
  }

  theta <- object$theta_hist
  if (is.null(theta) || !is.matrix(theta)) {
    stop(
      "ml_variable_aprendida does not contain a matrix theta_hist.",
      call. = FALSE
    )
  }

  X <- object$preparado$X
  if (is.null(X)) {
    stop("Source object does not contain preparado$X.", call. = FALSE)
  }

  p <- ncol(as.matrix(X))
  h <- as.integer(object$unidad$config$d_hidden)

  expected <- p * h + h + h + 1L
  if (ncol(theta) != expected) {
    stop(
      "theta_hist width does not match producer architecture: expected ",
      expected,
      ", observed ",
      ncol(theta),
      ".",
      call. = FALSE
    )
  }

  w1_grid <- expand.grid(
    input = seq_len(p),
    hidden = seq_len(h),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  w1_ids <- paste0(
    "W1[",
    w1_grid$input,
    ",",
    w1_grid$hidden,
    "]"
  )

  ids <- c(
    w1_ids,
    paste0("b1[", seq_len(h), "]"),
    paste0("W2[", seq_len(h), "]"),
    "b2"
  )
  layers <- c(
    rep("input_hidden", p * h),
    rep("hidden", h),
    rep("hidden_output", h),
    "output"
  )
  types <- c(
    rep("weight", p * h),
    rep("bias", h),
    rep("weight", h),
    "bias"
  )

  iters <- object$trayectoria$iter
  if (length(iters) != nrow(theta)) {
    stop(
      "theta_hist rows do not align with trajectory iterations.",
      call. = FALSE
    )
  }

  long <- data.frame(
    iter = rep(iters, each = ncol(theta)),
    layer = rep(layers, times = nrow(theta)),
    parameter_type = rep(types, times = nrow(theta)),
    parameter_id = rep(ids, times = nrow(theta)),
    value = as.vector(t(theta)),
    stringsAsFactors = FALSE
  )

  te_parameter_map(long)
}

#' Bridge a real ML.DSNeuralRNAS learned variable
#'
#' @param object ml_variable_aprendida.
#' @return te_trace with exact parameter map.
#' @export
te_bridge_ml_variable <- function(object) {
  if (!inherits(object, "ml_variable_aprendida")) {
    stop("Expected ml_variable_aprendida.", call. = FALSE)
  }

  tray <- as.data.frame(object$trayectoria)
  req <- c(
    "iter", "loss", "delta_loss", "grad_norm",
    "eta", "param_norm", "velocidad_parametrica"
  )
  missing <- setdiff(req, names(tray))
  if (length(missing)) {
    stop(
      "ml_variable_aprendida trajectory is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  pmap <- te_parameter_map_ml_variable(object)

  d <- data.frame(
    iter = tray$iter,
    loss = tray$loss,
    grad_norm = tray$grad_norm,
    eta = tray$eta,
    param_norm = tray$param_norm,
    param_velocity = tray$velocidad_parametrica,
    legacy_delta_loss = tray$delta_loss,
    stringsAsFactors = FALSE
  )

  tr <- te_trace(
    d,
    provenance = list(
      experiment_id = "ml-dsneuralrnas-runtime",
      source_package = "ML.DSNeuralRNAS",
      source_package_version =
        .te_pkg_version_or_unknown("ML.DSNeuralRNAS"),
      source_class = "ml_variable_aprendida",
      adapter_mode = "exact_ml_variable_bridge",
      dataset_id = "runtime-variable",
      model_id = paste0(
        "mlp-",
        ncol(as.matrix(object$preparado$X)),
        "-",
        object$unidad$config$d_hidden,
        "-1"
      ),
      optimizer = "gradient_descent",
      seed = object$unidad$config$seed,
      architecture = list(
        d_input = ncol(as.matrix(object$preparado$X)),
        d_hidden = object$unidad$config$d_hidden,
        d_output = 1L,
        activation = object$unidad$config$activation
      ),
      parameter_history_status = "available_exact_theta_hist",
      delta_loss_status = paste0(
        "source_preserved_as_legacy_delta_loss; ",
        "canonical_delta_loss_derived_as_Lk_minus_Lk_1"
      ),
      legacy_semantics_promoted = FALSE
    ),
    parameter_map = pmap
  )

  tr$architecture <- te_mlp_architecture(
    dims = c(
      ncol(as.matrix(object$preparado$X)),
      as.integer(object$unidad$config$d_hidden),
      1L
    ),
    activations = c(
      as.character(object$unidad$config$activation),
      "linear"
    ),
    model_id = tr$provenance$model_id
  )

  tr
}

#' Generate a small real DSNeuralRNAS MLP example
#'
#' This function executes the producer package. It is not a synthetic
#' TraceExplorer trace.
#'
#' @param T Training updates.
#' @param seed Seed.
#' @param d_hidden Hidden-layer width.
#' @param eta Learning rate.
#' @return rnas_mlp_train.
#' @export
te_example_rnas_mlp <- function(
    T = 120L,
    seed = 123L,
    d_hidden = 5L,
    eta = 0.04) {
  .te_require_namespace(
    "DSNeuralRNAS",
    "real DSNeuralRNAS MLP example"
  )

  set.seed(seed)
  x1 <- seq(-1, 1, length.out = 80)
  x2 <- cos(seq(0, 2 * pi, length.out = 80))
  X <- cbind(x1 = x1, x2 = x2)
  y <- 0.55 * x1 - 0.35 * x2 + 0.20 * sin(pi * x1)

  DSNeuralRNAS::rnas_train_mlp(
    X = X,
    y = y,
    d_hidden = as.integer(d_hidden),
    eta = as.numeric(eta),
    T = as.integer(T),
    activation = "tanh",
    init_sd = 0.1,
    seed = seed,
    registrar_cada = 1L
  )
}

#' Generate a small real ML.DSNeuralRNAS MLP variable example
#'
#' @param iter Training iterations.
#' @param seed Seed.
#' @param d_hidden Hidden-layer width.
#' @param eta Learning rate.
#' @return ml_variable_aprendida.
#' @export
te_example_ml_variable <- function(
    iter = 120L,
    seed = 123L,
    d_hidden = 5L,
    eta = 0.04) {
  .te_require_namespace(
    "ML.DSNeuralRNAS",
    "real ML.DSNeuralRNAS example"
  )

  set.seed(seed)
  x1 <- seq(-1, 1, length.out = 80)
  x2 <- cos(seq(0, 2 * pi, length.out = 80))
  X <- cbind(x1 = x1, x2 = x2)
  y <- 0.55 * x1 - 0.35 * x2 + 0.20 * sin(pi * x1)

  unidad <- ML.DSNeuralRNAS::ml_definir_unidad_variable(
    variable_objetivo = "y",
    entradas = c("x1", "x2"),
    rezagos = 0L,
    d_hidden = as.integer(d_hidden),
    activation = "tanh",
    eta = as.numeric(eta),
    iter = as.integer(iter),
    init_sd = 0.1,
    normalizar = FALSE,
    seed = seed
  )

  ML.DSNeuralRNAS::ml_aprender_variable(
    unidad,
    X = X,
    y = y
  )
}

#' Parameter trajectories from a trace parameter map
#'
#' @param trace te_trace with parameter_map.
#' @param layer Optional layer filter.
#' @param parameter_ids Optional explicit IDs.
#' @param max_parameters Maximum number when IDs are not supplied.
#' @return data.frame.
#' @export
te_parameter_trajectories <- function(
    trace,
    layer = NULL,
    parameter_ids = NULL,
    max_parameters = 12L) {

  if (!inherits(trace, "te_trace")) {
    stop("Expected te_trace.", call. = FALSE)
  }
  if (!inherits(trace$parameter_map, "te_parameter_map")) {
    stop(
      "This trace does not expose exact parameter history.",
      call. = FALSE
    )
  }

  d <- te_parameter_map_data(trace$parameter_map)

  if (!is.null(layer)) {
    d <- d[d$layer %in% layer, , drop = FALSE]
  }

  if (!is.null(parameter_ids)) {
    d <- d[d$parameter_id %in% parameter_ids, , drop = FALSE]
  } else {
    ids <- unique(d$parameter_id)
    ids <- head(ids, as.integer(max_parameters))
    d <- d[d$parameter_id %in% ids, , drop = FALSE]
  }

  d
}
