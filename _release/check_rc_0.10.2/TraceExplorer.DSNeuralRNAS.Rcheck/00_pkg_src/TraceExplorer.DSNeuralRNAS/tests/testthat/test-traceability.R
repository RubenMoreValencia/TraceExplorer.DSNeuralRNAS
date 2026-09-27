test_that("traceability index preserves iteration and provenance", {
  tr <- te_demo_mlp_trace(5)
  idx <- te_traceability_index(tr)

  expect_equal(idx$iter, 0:5)
  expect_true(all(c(
    "loss",
    "grad_norm",
    "eta",
    "param_norm",
    "param_velocity",
    "source_package",
    "source_class",
    "adapter_mode"
  ) %in% names(idx)))

  expect_true(all(idx$source_class == "didactic_generated_trace"))
  expect_true(all(idx$adapter_mode == "pedagogical_demo"))
})


test_that("traceability index can attach Psi Gamma Phi semantics", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  tr <- te_demo_mlp_trace(30)
  a <- te_run_formalspec(tr)
  idx <- te_traceability_index(tr, a)

  expect_true(any(!is.na(idx$window_id)))
  expect_true(any(!is.na(idx$candidate_regime)))
  expect_true(any(!is.na(idx$confirmed_regime)))
  expect_true("boundary_type" %in% names(idx))
})
