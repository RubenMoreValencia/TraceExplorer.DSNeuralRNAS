test_that("real ML multirun supports multiple architectures", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  mr <- te_example_multirun_ml(
    iter = 12L,
    seeds = c(101L, 202L),
    d_hidden = c(3L, 5L),
    eta = 0.04
  )

  expect_s3_class(mr, "te_multirun")

  arch <- te_multirun_architectures(mr)

  expect_true(all(arch$architecture_available))
  expect_equal(arch$n_layers, c(2L, 2L))
  expect_true(
    length(unique(arch$n_parameters)) == 2L
  )

  d <- te_multirun_trace_data(mr)
  expect_equal(
    length(unique(d$run_id)),
    2L
  )
})

test_that("real ML multirun can use the frozen FormalSpec protocol", {
  skip_if_not_installed("ML.DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  mr <- te_example_multirun_ml(
    iter = 15L,
    seeds = c(101L, 202L),
    d_hidden = c(3L, 5L)
  )

  fs <- te_multirun_formalspec(mr)
  s <- te_multirun_formalspec_summary(fs)

  expect_equal(nrow(s), 2L)
  expect_true(all(
    s$formal_spec_version == "1.0.0"
  ))
  expect_true(all(
    s$calibration_on_trace == FALSE
  ))
})
