.te_bridge_dataframe <- function(object, provenance = list(), ...) {
  te_trace(
    as.data.frame(object),
    provenance = modifyList(
      list(
        source_class = "data.frame",
        source_package = NA_character_,
        adapter_mode = "canonical_dataframe"
      ),
      provenance
    )
  )
}

.te_bridge_dsfs_trace <- function(object, ...) {
  .te_require_namespace(
    "DSNeuralRNAS.FormalSpec",
    "direct dsfs_trace import"
  )
  d <- DSNeuralRNAS.FormalSpec::dsfs_trace_data(object)
  te_trace(
    d,
    provenance = list(
      source_class = "dsfs_trace",
      source_package = "DSNeuralRNAS.FormalSpec",
      adapter_mode = "direct_formalspec_trace"
    )
  )
}

.te_bridge_rnas_neuron_train <- function(object, ...) {
  .te_require_namespace(
    "DSNeuralRNAS.FormalSpec",
    "rnas_neuron_train frozen adapter"
  )
  engine_version <- if (
    requireNamespace("DSNeuralRNAS", quietly = TRUE)
  ) {
    as.character(
      utils::packageVersion("DSNeuralRNAS")
    )
  } else {
    "unknown"
  }

  md <- DSNeuralRNAS.FormalSpec::dsfs_metadata(
    experiment_id = "TraceExplorer-import",
    engine = "DSNeuralRNAS",
    engine_version = engine_version,
    seed = NA_integer_,
    dataset_id = "source-object-import",
    model_id = "rnas_neuron_train",
    optimizer = "source_defined",
    notes = paste0(
      "TraceExplorer source-object bridge. ",
      "The source RDS does not by itself establish the article's ",
      "original FormalSpec semantic pipeline."
    )
  )
  tr <- DSNeuralRNAS.FormalSpec::dsfs_adapt(
    object,
    metadata = md
  )
  d <- DSNeuralRNAS.FormalSpec::dsfs_trace_data(tr)
  te_trace(
    d,
    provenance = list(
      experiment_id = "TraceExplorer-import",
      source_class = "rnas_neuron_train",
      source_package = "DSNeuralRNAS",
      source_package_version = engine_version,
      adapter_mode = "frozen_formalspec_adapter",
      dataset_id = "source-object-import",
      model_id = "rnas_neuron_train",
      optimizer = "source_defined",
      source_semantics = "bridgeable_source_object",
      original_article_semantics_present = FALSE,
      legacy_semantics_promoted = FALSE
    )
  )
}

.te_bridge_control_eta <- function(object, ...) {
  tray <- object$trayectoria
  if (!is.data.frame(tray)) {
    stop(
      "rnas_control_eta_train requires $trayectoria.",
      call. = FALSE
    )
  }

  aliases <- list(
    iter = c("iter", "iteration", "k"),
    loss = c("loss", "perdida", "L"),
    grad_norm = c("grad_norm", "gradient_norm", "norma_gradiente"),
    eta = c("eta", "learning_rate")
  )

  find_name <- function(cands, required = FALSE) {
    hit <- cands[cands %in% names(tray)]
    if (length(hit)) return(hit[[1L]])
    if (required) {
      stop(
        "Missing required source field among aliases: ",
        paste(cands, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    NULL
  }

  n_iter <- find_name(aliases$iter, TRUE)
  n_loss <- find_name(aliases$loss, TRUE)
  n_grad <- find_name(aliases$grad_norm, FALSE)
  n_eta <- find_name(aliases$eta, FALSE)

  d <- data.frame(
    iter = tray[[n_iter]],
    loss = tray[[n_loss]],
    stringsAsFactors = FALSE
  )
  if (!is.null(n_grad)) d$grad_norm <- tray[[n_grad]]
  if (!is.null(n_eta)) d$eta <- tray[[n_eta]]

  pcols <- grep(
    "^(theta|w|b)[._]?[0-9]+$",
    names(tray),
    value = TRUE
  )
  if (length(pcols)) {
    for (nm in pcols) d[[nm]] <- tray[[nm]]
  }

  for (nm in c("speed", "regimen", "accion_eta", "reduccion_relativa")) {
    if (nm %in% names(tray)) {
      target <- switch(
        nm,
        speed = "legacy_speed",
        regimen = "legacy_regime",
        accion_eta = "legacy_eta_action",
        reduccion_relativa = "legacy_relative_reduction"
      )
      d[[target]] <- tray[[nm]]
    }
  }

  te_trace(
    d,
    provenance = list(
      source_class = "rnas_control_eta_train",
      source_package = "DSNeuralRNAS",
      adapter_mode = "explicit_traceexplorer_bridge",
      legacy_semantics_promoted = FALSE
    )
  )
}

.te_extract_ml_trace <- function(object) {
  candidates <- c(
    "trayectoria",
    "trayectoria_aprendizaje",
    "trajectory",
    "trace"
  )
  nm <- candidates[candidates %in% names(object)]
  if (!length(nm)) {
    stop(
      "ML object does not expose a recognized trajectory field.",
      call. = FALSE
    )
  }
  object[[nm[[1L]]]]
}

.te_bridge_ml_variable <- function(object, ...) {
  tray <- .te_extract_ml_trace(object)
  if (!is.data.frame(tray)) {
    stop("ML trajectory must be a data.frame.", call. = FALSE)
  }

  aliases <- list(
    iter = c("iter", "iteration", "k"),
    loss = c("loss", "perdida", "L"),
    delta_loss = c("delta_loss", "variacion_perdida"),
    grad_norm = c("grad_norm", "norma_gradiente"),
    eta = c("eta", "learning_rate"),
    param_norm = c("param_norm", "norma_parametrica"),
    param_velocity = c("param_velocity", "velocidad_parametrica")
  )

  out <- list()
  for (target in names(aliases)) {
    src <- aliases[[target]][aliases[[target]] %in% names(tray)]
    if (length(src)) out[[target]] <- tray[[src[[1L]]]]
  }

  if (is.null(out$iter) || is.null(out$loss)) {
    stop(
      "ML bridge requires iter and loss aliases.",
      call. = FALSE
    )
  }

  te_trace(
    as.data.frame(out, stringsAsFactors = FALSE),
    provenance = list(
      source_class = class(object)[[1L]],
      source_package = "ML.DSNeuralRNAS",
      adapter_mode = "ml_trace_bridge"
    )
  )
}

#' Register default ecosystem bridges
#'
#' @export
te_register_default_bridges <- function() {
  te_register_bridge(
    "data.frame",
    .te_bridge_dataframe,
    source_package = NA_character_,
    description = "Canonical data.frame contract.",
    normative = FALSE
  )

  te_register_bridge(
    "dsfs_trace",
    .te_bridge_dsfs_trace,
    source_package = "DSNeuralRNAS.FormalSpec",
    description = "Direct import of a canonical FormalSpec trace.",
    normative = TRUE
  )

  te_register_bridge(
    "rnas_neuron_train",
    .te_bridge_rnas_neuron_train,
    source_package = "DSNeuralRNAS",
    description = "Frozen FormalSpec adapter path.",
    normative = TRUE
  )

  te_register_bridge(
    "rnas_control_eta_train",
    .te_bridge_control_eta,
    source_package = "DSNeuralRNAS",
    description = "Explicit auditable bridge; legacy regime remains non-normative.",
    normative = FALSE
  )

  te_register_bridge(
    "rnas_mlp_train",
    te_bridge_rnas_mlp,
    source_package = "DSNeuralRNAS",
    description = paste0(
      "Real MLP bridge. Reconstructs global parameter norm exactly from ",
      "producer block norms; parameter velocity remains unavailable."
    ),
    normative = FALSE
  )

  te_register_bridge(
    "ml_variable_aprendida",
    te_bridge_ml_variable,
    source_package = "ML.DSNeuralRNAS",
    description = paste0(
      "Exact learned-variable MLP bridge with global dynamics and ",
      "theta_hist parameter map."
    ),
    normative = FALSE
  )

  te_register_bridge(
    "ml_componentes_dinamicos",
    .te_bridge_ml_variable,
    source_package = "ML.DSNeuralRNAS",
    description = "Legacy generic ML.DSNeuralRNAS trajectory bridge.",
    normative = FALSE
  )

  invisible(te_bridge_registry())
}
