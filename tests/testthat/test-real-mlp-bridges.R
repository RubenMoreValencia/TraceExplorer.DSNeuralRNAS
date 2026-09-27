test_that("real DSNeuralRNAS MLP bridge preserves source loss and gradient", {
  skip_if_not_installed("DSNeuralRNAS")

  src <- te_example_rnas_mlp(T = 30L, seed = 123L)
  tr <- te_bridge_rnas_mlp(src)
  d <- te_trace_data(tr)

  expect_s3_class(src, "rnas_mlp_train")
  expect_s3_class(tr, "te_trace")
  expect_equal(d$loss, src$trayectoria$loss)
  expect_equal(d$grad_norm, src$trayectoria$grad_norm)
  expect_equal(
    d$eta,
    rep(src$configuracion$eta, nrow(src$trayectoria))
  )

  expected_norm <- sqrt(
    src$trayectoria$W_norm^2 +
      src$trayectoria$b1_norm^2 +
      src$trayectoria$v_norm^2 +
      src$trayectoria$b2^2
  )
  expect_equal(d$param_norm, expected_norm)
  expect_false("param_velocity" %in% names(d))

  cap <- te_capabilities(tr)
  expect_equal(cap$capability, "global_dynamics")
  expect_false(cap$parameter_velocity)
  expect_false(cap$parameter_history)
})

test_that("real ML.DSNeuralRNAS bridge exposes exact theta history", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  src <- te_example_ml_variable(iter = 30L, seed = 123L)
  tr <- te_bridge_ml_variable(src)
  d <- te_trace_data(tr)

  expect_s3_class(src, "ml_variable_aprendida")
  expect_s3_class(tr, "te_trace")
  expect_equal(d$loss, src$trayectoria$loss)
  expect_equal(d$grad_norm, src$trayectoria$grad_norm)
  expect_equal(d$eta, src$trayectoria$eta)
  expect_equal(d$param_norm, src$trayectoria$param_norm)
  expect_equal(
    d$param_velocity,
    src$trayectoria$velocidad_parametrica
  )

  # Producer convention is preserved as evidence but is not promoted to the
  # FormalSpec canonical delta_loss field.
  expect_equal(
    d$legacy_delta_loss,
    src$trayectoria$delta_loss
  )
  expect_equal(
    d$delta_loss,
    c(NA_real_, diff(d$loss))
  )

  expect_s3_class(tr$parameter_map, "te_parameter_map")
  pm <- te_parameter_map_data(tr$parameter_map)
  expect_equal(
    nrow(pm),
    nrow(src$theta_hist) * ncol(src$theta_hist)
  )

  cap <- te_capabilities(tr)
  expect_equal(cap$capability, "parameter_history")
  expect_true(cap$parameter_velocity)
  expect_true(cap$parameter_history)
})

test_that("ML parameter map reconstructs each theta_hist row exactly", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  src <- te_example_ml_variable(iter = 12L, seed = 123L)
  tr <- te_bridge_ml_variable(src)
  pm <- te_parameter_map_data(tr$parameter_map)

  for (i in seq_len(nrow(src$theta_hist))) {
    k <- src$trayectoria$iter[[i]]
    got <- pm$value[pm$iter == k]
    expect_equal(got, as.numeric(src$theta_hist[i, ]))
  }
})

test_that("both real MLP bridges can feed FormalSpec without recalibration", {
  skip_if_not_installed("DSNeuralRNAS")
  skip_if_not_installed("ML.DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  rnas <- te_bridge_rnas_mlp(
    te_example_rnas_mlp(T = 30L, seed = 123L)
  )
  ml <- te_bridge_ml_variable(
    te_example_ml_variable(iter = 30L, seed = 123L)
  )

  ml_data <- te_trace_data(ml)
  expect_equal(
    ml_data$delta_loss,
    c(NA_real_, diff(ml_data$loss))
  )
  expect_equal(
    ml_data$legacy_delta_loss,
    te_example_ml_variable(iter = 30L, seed = 123L)$trayectoria$delta_loss
  )

  a1 <- te_run_formalspec(rnas)
  a2 <- te_run_formalspec(ml)

  expect_true(all(
    te_validate_formalspec_analysis(a1)$status == "PASS"
  ))
  expect_true(all(
    te_validate_formalspec_analysis(a2)$status == "PASS"
  ))
  expect_false(te_formalspec_summary(a1)$calibration_on_trace)
  expect_false(te_formalspec_summary(a2)$calibration_on_trace)
})
