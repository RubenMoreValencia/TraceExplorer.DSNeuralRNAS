test_that("multilayer demo decomposes global parameter norm exactly", {
  tr <- te_demo_multilayer_trace(
    dims = c(3L, 6L, 4L, 1L),
    n_updates = 20L,
    seed = 321L
  )

  expect_s3_class(tr$architecture, "te_architecture")
  expect_s3_class(tr$parameter_map, "te_parameter_map")

  chk <- te_verify_parameter_decomposition(tr)
  expect_true(chk$exact_within_1e_12)
  expect_lte(chk$max_abs_error, 1e-12)
})

test_that("block and layer dynamics are reconstructable", {
  tr <- te_demo_multilayer_trace(
    dims = c(2L, 4L, 3L, 1L),
    n_updates = 10L,
    seed = 321L
  )

  b <- te_parameter_block_dynamics(tr)
  l <- te_layer_dynamics(tr)

  expect_true(all(c(
    "block_norm",
    "block_velocity"
  ) %in% names(b)))

  expect_true(all(c(
    "layer_param_norm",
    "layer_velocity"
  ) %in% names(l)))

  expect_equal(
    length(unique(b$layer)),
    3L
  )
  expect_equal(
    length(unique(l$layer)),
    3L
  )
})

test_that("real ML variable bridge exposes architecture contract", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  src <- te_example_ml_variable(iter = 20L, seed = 123L)
  tr <- te_bridge_ml_variable(src)

  expect_s3_class(tr$architecture, "te_architecture")
  expect_true(
    te_capabilities(tr)$architecture_contract
  )

  chk <- te_verify_parameter_decomposition(tr)
  expect_true(chk$exact_within_1e_12)
})
