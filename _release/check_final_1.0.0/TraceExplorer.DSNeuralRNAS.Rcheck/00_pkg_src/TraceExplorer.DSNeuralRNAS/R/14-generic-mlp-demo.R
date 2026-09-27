#' Construct a deterministic arbitrary-depth MLP trace for architecture teaching
#'
#' This is an architecture/parameter-history demonstration, not a trained
#' DSNeuralRNAS model. Real producer bridges remain the evidence path.
#'
#' @param dims Network dimensions c(input, hidden..., output).
#' @param n_updates Number of updates.
#' @param seed Seed.
#' @return te_trace with architecture and parameter map.
#' @export
te_demo_multilayer_trace <- function(
    dims = c(3L, 6L, 4L, 1L),
    n_updates = 60L,
    seed = 321L) {

  arch <- te_mlp_architecture(
    dims = dims,
    activations = c(
      rep("tanh", length(dims) - 2L),
      "linear"
    ),
    model_id = paste0(
      "didactic-mlp-",
      paste(dims, collapse = "x")
    )
  )

  set.seed(seed)
  K <- as.integer(n_updates)
  iters <- 0:K

  layer_defs <- te_architecture_data(arch)
  p_rows <- list()
  q <- 1L

  for (ell in seq_len(nrow(layer_defs))) {
    in_dim <- layer_defs$input_dim[[ell]]
    out_dim <- layer_defs$output_dim[[ell]]
    lid <- layer_defs$layer_id[[ell]]

    W0 <- matrix(
      stats::rnorm(in_dim * out_dim, sd = 0.12),
      nrow = in_dim,
      ncol = out_dim
    )
    b0 <- stats::rnorm(out_dim, sd = 0.05)

    w_ids <- as.vector(
      outer(
        seq_len(in_dim),
        seq_len(out_dim),
        function(i, j) paste0(
          "W", ell, "[", i, ",", j, "]"
        )
      )
    )
    b_ids <- paste0(
      "b", ell, "[", seq_len(out_dim), "]"
    )

    for (i in seq_along(iters)) {
      k <- iters[[i]]
      decay <- exp(-0.025 * k)
      drift <- 1 - decay

      Wk <- W0 * decay +
        0.02 * drift * sign(W0 + 1e-12)
      bk <- b0 * decay

      p_rows[[q]] <- data.frame(
        iter = k,
        layer = lid,
        parameter_type = "weight",
        parameter_id = w_ids,
        value = as.vector(Wk),
        stringsAsFactors = FALSE
      )
      q <- q + 1L

      p_rows[[q]] <- data.frame(
        iter = k,
        layer = lid,
        parameter_type = "bias",
        parameter_id = b_ids,
        value = as.vector(bk),
        stringsAsFactors = FALSE
      )
      q <- q + 1L
    }
  }

  pm <- te_parameter_map(do.call(rbind, p_rows))
  pm_data <- te_parameter_map_data(pm)

  norm_by_iter <- vapply(
    split(pm_data$value, pm_data$iter),
    function(v) sqrt(sum(v^2)),
    numeric(1)
  )

  velocity <- rep(NA_real_, length(iters))
  for (i in 2:length(iters)) {
    a <- pm_data[
      pm_data$iter == iters[[i - 1L]],
      ,
      drop = FALSE
    ]
    b <- pm_data[
      pm_data$iter == iters[[i]],
      ,
      drop = FALSE
    ]
    a <- a[order(a$layer, a$parameter_type, a$parameter_id), ]
    b <- b[order(b$layer, b$parameter_type, b$parameter_id), ]
    velocity[[i]] <- sqrt(sum((b$value - a$value)^2))
  }

  loss <- 0.85 * exp(-0.05 * iters) + 0.025
  grad <- 1.10 * exp(-0.045 * iters) + 0.015

  d <- data.frame(
    iter = iters,
    loss = loss,
    grad_norm = grad,
    eta = rep(0.03, length(iters)),
    param_norm = unname(norm_by_iter[as.character(iters)]),
    param_velocity = velocity,
    stringsAsFactors = FALSE
  )

  tr <- te_trace(
    d,
    provenance = list(
      experiment_id = "didactic-multilayer-architecture",
      source_package = "TraceExplorer.DSNeuralRNAS",
      source_package_version = "0.4.0",
      source_class = "didactic_multilayer_trace",
      adapter_mode = "pedagogical_architecture_demo",
      dataset_id = "none",
      model_id = arch$model_id,
      optimizer = "deterministic_decay_demo",
      seed = seed,
      disclaimer = paste0(
        "Architecture teaching trace; not a real ",
        "DSNeuralRNAS training result."
      )
    ),
    parameter_map = pm
  )

  tr$architecture <- arch
  tr
}
