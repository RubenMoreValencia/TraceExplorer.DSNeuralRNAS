test_that("didactic trace supports three generic 3D spaces", {
  tr <- te_demo_mlp_trace(20)
  expect_equal(nrow(te_learning_space(tr)), 21)
  expect_equal(nrow(te_parameter_space(tr)), 21)
  expect_equal(nrow(te_control_space(tr)), 21)
})
