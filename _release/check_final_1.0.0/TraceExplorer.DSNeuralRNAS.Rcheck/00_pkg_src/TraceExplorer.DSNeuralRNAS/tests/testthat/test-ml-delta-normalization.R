test_that("ML source delta is preserved while canonical delta follows FormalSpec", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  src <- te_example_ml_variable(iter = 20L, seed = 123L)
  tr <- te_bridge_ml_variable(src)
  d <- te_trace_data(tr)

  expect_equal(
    d$legacy_delta_loss,
    src$trayectoria$delta_loss
  )

  canonical <- c(NA_real_, diff(d$loss))
  expect_equal(d$delta_loss, canonical)

  # The bridge must not silently overwrite producer evidence.
  expect_true("legacy_delta_loss" %in% names(d))
  expect_true("delta_loss" %in% names(d))
})

test_that("ML canonical delta can feed frozen FormalSpec", {
  skip_if_not_installed("ML.DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  src <- te_example_ml_variable(iter = 30L, seed = 123L)
  tr <- te_bridge_ml_variable(src)

  fs_trace <- te_to_formalspec(tr)
  expect_s3_class(fs_trace, "dsfs_trace")

  fs <- te_run_formalspec(tr)
  expect_s3_class(fs, "te_formalspec_analysis")
  expect_true(all(
    te_validate_formalspec_analysis(fs)$status == "PASS"
  ))
})
