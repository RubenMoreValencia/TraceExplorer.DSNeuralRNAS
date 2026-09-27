test_that("multirun preserves independent traces and identifiers", {
  a <- te_demo_mlp_trace(10)
  b <- te_demo_multilayer_trace(
    dims = c(2L, 4L, 1L),
    n_updates = 8L,
    seed = 321L
  )

  mr <- te_multirun(
    list(a, b),
    run_ids = c("A", "B"),
    labels = c("simple", "multilayer")
  )

  expect_s3_class(mr, "te_multirun")
  expect_equal(mr$run_ids, c("A", "B"))

  val <- te_validate_multirun(mr)
  expect_true(all(val$status == "PASS"))
})

test_that("normalized-time comparison does not interpolate rows", {
  a <- te_demo_mlp_trace(10)
  b <- te_demo_mlp_trace(20)

  mr <- te_multirun(
    list(a, b),
    run_ids = c("A", "B")
  )

  d <- te_multirun_trace_data(mr)

  expect_equal(
    nrow(d),
    nrow(te_trace_data(a)) +
      nrow(te_trace_data(b))
  )

  by_run <- split(d, d$run_id)

  expect_equal(range(by_run$A$tau), c(0, 1))
  expect_equal(range(by_run$B$tau), c(0, 1))
  expect_equal(by_run$A$iter, te_trace_data(a)$iter)
  expect_equal(by_run$B$iter, te_trace_data(b)$iter)
})

test_that("multirun index is descriptive and retains architecture metadata", {
  a <- te_demo_multilayer_trace(
    dims = c(2L, 3L, 1L),
    n_updates = 8L
  )
  b <- te_demo_multilayer_trace(
    dims = c(2L, 5L, 1L),
    n_updates = 8L
  )

  mr <- te_multirun(
    list(a, b),
    run_ids = c("A", "B")
  )

  idx <- te_multirun_index(mr)
  arch <- te_multirun_architectures(mr)

  expect_equal(nrow(idx), 2L)
  expect_equal(nrow(arch), 2L)
  expect_true(all(arch$architecture_available))
  expect_false(
    identical(
      arch$n_parameters[[1L]],
      arch$n_parameters[[2L]]
    )
  )
  expect_true(all(c(
    "loss_initial",
    "loss_final",
    "relative_loss_reduction"
  ) %in% names(idx)))
})

test_that("run ids must be unique", {
  a <- te_demo_mlp_trace(5)
  b <- te_demo_mlp_trace(5)

  expect_error(
    te_multirun(
      list(a, b),
      run_ids = c("same", "same")
    ),
    "unique"
  )
})
