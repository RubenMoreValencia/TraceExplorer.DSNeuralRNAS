test_that("generic MLP architecture supports arbitrary depth", {
  arch <- te_mlp_architecture(
    dims = c(4L, 8L, 6L, 3L, 1L),
    activations = c("relu", "tanh", "tanh", "linear"),
    model_id = "deep-demo"
  )

  expect_s3_class(arch, "te_architecture")
  expect_equal(arch$n_layers, 4L)

  val <- te_validate_architecture(arch)
  expect_true(all(val$status == "PASS"))

  d <- te_architecture_data(arch)
  expect_equal(d$input_dim, c(4L, 8L, 6L, 3L))
  expect_equal(d$output_dim, c(8L, 6L, 3L, 1L))
})

test_that("parameter count matches dense MLP formula", {
  arch <- te_mlp_architecture(
    dims = c(3L, 5L, 2L, 1L),
    activations = c("tanh", "tanh", "linear")
  )

  expected <- 3 * 5 + 5 + 5 * 2 + 2 + 2 * 1 + 1
  expect_equal(arch$n_parameters, expected)
})
