test_that("FormalSpec is applied independently to every run", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_demo_mlp_trace(15)
  b <- te_demo_multilayer_trace(
    dims = c(2L, 4L, 1L),
    n_updates = 15L
  )

  mr <- te_multirun(
    list(a, b),
    run_ids = c("A", "B")
  )

  fs <- te_multirun_formalspec(mr)

  expect_s3_class(
    fs,
    "te_multirun_formalspec"
  )

  val <- te_validate_multirun_formalspec(fs)
  expect_true(all(val$status == "PASS"))

  s <- te_multirun_formalspec_summary(fs)
  expect_equal(nrow(s), 2L)
  expect_true(all(
    s$freeze_id ==
      "DSFS-1.0.0-FIRST-ARTICLE"
  ))
  expect_true(all(!s$calibration_on_trace))
})

test_that("semantic comparison preserves run identity", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  mr <- te_multirun(
    list(
      te_demo_mlp_trace(15),
      te_demo_mlp_trace(18)
    ),
    run_ids = c("A", "B")
  )

  fs <- te_multirun_formalspec(mr)
  d <- te_multirun_semantic_data(fs)

  expect_true(all(c("A", "B") %in% unique(d$run_id)))
  expect_true(all(c(
    "candidate_regime",
    "confirmed_regime",
    "boundary_type"
  ) %in% names(d)))
})
