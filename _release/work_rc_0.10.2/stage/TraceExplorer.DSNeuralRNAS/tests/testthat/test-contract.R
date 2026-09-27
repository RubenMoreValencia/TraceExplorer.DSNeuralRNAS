test_that("minimal trace contract accepts iter and loss", {
  x <- data.frame(iter = 0:3, loss = c(1, .8, .7, .6))
  expect_s3_class(te_trace(x), "te_trace")
})

test_that("trace contract rejects non-increasing iteration", {
  x <- data.frame(iter = c(0, 2, 1), loss = c(1, .8, .7))
  expect_error(te_trace(x), "validation failed")
})

test_that("parameter fields derive norm and velocity", {
  x <- data.frame(
    iter = 0:2,
    loss = c(1, .8, .6),
    theta1 = c(1, .8, .7),
    theta2 = c(0, .1, .2)
  )
  tr <- te_trace(x)
  d <- te_trace_data(tr)
  expect_true(all(c("param_norm", "param_velocity") %in% names(d)))
  expect_true(is.na(d$param_velocity[[1L]]))
})
