test_that("FormalSpec frozen baseline preflight is valid", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  pf <- te_formalspec_preflight(error = FALSE)
  expect_true(all(pf$status == "PASS"))
})

test_that("TraceExplorer trace converts to FormalSpec 1.0.0", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  tr <- te_demo_mlp_trace(20)
  fs <- te_to_formalspec(tr)

  expect_s3_class(fs, "dsfs_trace")
  expect_equal(fs$spec$version, "1.0.0")

  src <- te_trace_data(tr)
  dst <- DSNeuralRNAS.FormalSpec::dsfs_trace_data(fs)

  expect_equal(dst$loss, src$loss)
  expect_equal(dst$grad_norm, src$grad_norm)
  expect_equal(dst$eta, src$eta)
})

test_that("complete Psi Gamma Phi pipeline validates", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  tr <- te_demo_mlp_trace(30)
  a <- te_run_formalspec(tr)

  expect_s3_class(a, "te_formalspec_analysis")

  val <- te_validate_formalspec_analysis(a)
  expect_true(all(val$status == "PASS"))

  s <- te_formalspec_summary(a)
  expect_equal(s$n_trace_rows, 31)
  expect_equal(s$n_windows, 29)
  expect_false(s$calibration_on_trace)
  expect_equal(
    s$freeze_id,
    "DSFS-1.0.0-FIRST-ARTICLE"
  )
})

test_that("regime feature space uses exact FormalSpec window features", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  tr <- te_demo_mlp_trace(30)
  a <- te_run_formalspec(tr)
  sp <- te_regime_feature_space(a)

  expect_equal(nrow(sp), 29)
  expect_true(all(c(
    "loss_slope",
    "loss_mean_abs_change",
    "loss_oscillation_rate"
  ) %in% names(
    DSNeuralRNAS.FormalSpec::dsfs_windows_data(a$windows)
  )))
  expect_true(all(c(
    "candidate_regime",
    "confirmed_regime"
  ) %in% names(sp)))
})
