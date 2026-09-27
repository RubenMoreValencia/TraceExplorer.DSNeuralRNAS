test_that("loss comparison supports absolute, relative and log-relative views", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(10)),
    source_name = "E1B_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(12)),
    source_name = "E1B_B.rds"
  )

  set <- te_article_case_set(list(a, b))

  abs_d <- te_article_case_set_loss_view(set, "absolute")
  rel_d <- te_article_case_set_loss_view(set, "relative")
  log_d <- te_article_case_set_loss_view(set, "log_relative")

  expect_equal(abs_d$display_value, abs_d$loss)

  first_rel <- tapply(
    rel_d$display_value,
    rel_d$case_key,
    function(v) v[[1L]]
  )
  expect_true(all(abs(first_rel - 1) <= 1e-12))

  first_log <- tapply(
    log_d$display_value,
    log_d$case_key,
    function(v) v[[1L]]
  )
  expect_true(all(abs(first_log) <= 1e-12))
})

test_that("loss view does not change original row count or iterations", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(10)),
    source_name = "E1B_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(20)),
    source_name = "E4A_B.rds"
  )

  set <- te_article_case_set(list(a, b))

  raw <- te_article_case_set_trace_data(set)
  log_d <- te_article_case_set_loss_view(set, "log_relative")

  expect_equal(nrow(log_d), nrow(raw))
  expect_equal(log_d$iter, raw$iter)
  expect_equal(log_d$loss, raw$loss)
})

test_that("boundary overlay uses exact original iterations", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(18)),
    source_name = "E1B_A.rds"
  )

  set <- te_article_case_set(list(a))

  ev <- te_article_case_set_boundary_overlay(
    set,
    "log_relative"
  )

  if (nrow(ev)) {
    d <- te_article_case_set_loss_view(
      set,
      "log_relative"
    )

    expect_true(all(ev$end_iter %in% d$iter))
    expect_true(all(is.finite(ev$tau)))
    expect_true(all(is.finite(ev$display_value)))
  } else {
    succeed()
  }
})

test_that("loss summary reports ratios without ranking", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(10)),
    source_name = "E1B_A.rds"
  )

  set <- te_article_case_set(list(a))
  s <- te_article_case_set_loss_summary(set)

  expect_true("final_initial_ratio" %in% names(s))
  expect_true("log_final_initial_ratio" %in% names(s))
  expect_false(any(c("rank", "winner", "score") %in% names(s)))
})
