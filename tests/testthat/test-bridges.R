test_that("data.frame bridge is registered", {
  te_register_default_bridges()
  reg <- te_bridge_registry()
  expect_true("data.frame" %in% reg$source_class)
})

test_that("data.frame bridge produces te_trace", {
  te_register_default_bridges()
  x <- data.frame(iter = 0:2, loss = c(1, .8, .6))
  expect_s3_class(te_bridge(x), "te_trace")
})
