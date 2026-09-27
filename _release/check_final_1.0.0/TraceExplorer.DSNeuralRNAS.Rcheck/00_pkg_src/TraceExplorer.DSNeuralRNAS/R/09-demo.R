#' Didactic MLP-like learning trace
#'
#' This generator is not presented as a trained DSNeuralRNAS model. It creates
#' a deterministic pedagogical trace with multiple parameter coordinates so the
#' TraceExplorer interface can be exercised before loading a real RDS/CSV.
#'
#' @param n_updates Number of updates.
#' @param eta Learning rate.
#' @return te_trace.
#' @export
te_demo_mlp_trace <- function(n_updates = 80L, eta = 0.04) {
  n_updates <- as.integer(n_updates)
  k <- 0:n_updates

  loss <- 0.9 * exp(-0.055 * k) + 0.035
  grad <- 1.3 * exp(-0.045 * k) + 0.02
  theta1 <- 0.9 - 0.55 * (1 - exp(-0.05 * k))
  theta2 <- -0.7 + 0.48 * (1 - exp(-0.04 * k))
  theta3 <- 0.25 + 0.22 * (1 - exp(-0.035 * k))
  theta4 <- -0.15 + 0.11 * (1 - exp(-0.06 * k))

  d <- data.frame(
    iter = k,
    loss = loss,
    grad_norm = grad,
    eta = rep(eta, length(k)),
    theta1 = theta1,
    theta2 = theta2,
    theta3 = theta3,
    theta4 = theta4,
    stringsAsFactors = FALSE
  )

  te_trace(
    d,
    provenance = list(
      experiment_id = "didactic-mlp-demo",
      source_package = "TraceExplorer.DSNeuralRNAS",
      source_package_version = "0.2.0",
      source_class = "didactic_generated_trace",
      adapter_mode = "pedagogical_demo",
      dataset_id = "deterministic-didactic-demo",
      model_id = "mlp-like-four-parameter-demo",
      optimizer = "synthetic_gradient_descent_like",
      disclaimer = paste0(
        "Deterministic didactic trace for interface exploration; ",
        "not a DSNeuralRNAS training result."
      )
    )
  )
}
