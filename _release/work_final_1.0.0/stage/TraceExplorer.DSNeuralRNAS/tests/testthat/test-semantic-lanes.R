test_that("semantic lanes keep scientific cases separate", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_050.rds"
  )
  a$experiment <- "E1"
  a$stage <- "E1B"
  a$case_id <- "E1B_050"

  b <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_070.rds"
  )
  b$experiment <- "E1"
  b$stage <- "E1B"
  b$case_id <- "E1B_070"

  set <- te_article_case_set(list(a, b))

  d <- te_article_case_set_semantic_lanes(set)

  expect_equal(
    length(unique(d$display_case)),
    2L
  )
  expect_equal(
    length(unique(d$lane_id)),
    2L
  )

  by_case <- split(d$lane_id, d$display_case)

  expect_true(all(vapply(
    by_case,
    function(v) length(unique(v)) == 1L,
    logical(1)
  )))
})

test_that("semantic lane helper preserves FormalSpec values exactly", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_CASE.rds"
  )

  set <- te_article_case_set(list(a))

  raw <- te_article_case_set_semantic_timeline(set)
  lanes <- te_article_case_set_semantic_lanes(set)

  expect_equal(lanes$window_id, raw$window_id)
  expect_equal(lanes$start_iter, raw$start_iter)
  expect_equal(lanes$end_iter, raw$end_iter)
  expect_equal(lanes$tau, raw$tau)
  expect_equal(
    lanes$candidate_regime,
    raw$candidate_regime
  )
  expect_equal(
    lanes$confirmed_regime,
    raw$confirmed_regime
  )
})
