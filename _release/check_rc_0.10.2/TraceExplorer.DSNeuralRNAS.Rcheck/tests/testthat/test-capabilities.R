test_that("didactic trace reports parametric dynamics", {
  tr <- te_demo_mlp_trace(10)
  cap <- te_capabilities(tr)

  expect_equal(cap$capability, "parametric_dynamics")
  expect_true(cap$loss)
  expect_true(cap$gradient_norm)
  expect_true(cap$learning_rate)
  expect_true(cap$parameter_norm)
  expect_true(cap$parameter_velocity)
  expect_false(cap$parameter_history)
})
