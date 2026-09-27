test_that("semantic timeline uses existing article analyses only", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  dynamic <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_DYNAMIC.rds"
  )

  source_only <- structure(
    list(
      trace = te_demo_mlp_trace(18),
      analysis = NULL,
      source_object = NULL,
      classification = data.frame(
        role = "source_object",
        dynamic_eligible = TRUE,
        bridge_eligible = TRUE,
        object_class = "synthetic_source",
        stringsAsFactors = FALSE
      ),
      experiment = "E5",
      stage = "E5A",
      case_id = "E5_SOURCE",
      source_name = "E5_SOURCE.rds",
      source_path = NA_character_,
      source_md5 = NA_character_,
      uses_existing_article_semantics = FALSE
    ),
    class = "te_article_case"
  )

  set <- te_article_case_set(
    list(dynamic, source_only)
  )

  d <- te_article_case_set_semantic_timeline(set)

  expect_true(nrow(d) > 0L)
  expect_true(all(
    d$case_id != "E5_SOURCE"
  ))
  expect_true(all(
    d$semantics_origin ==
      "existing_article_pipeline"
  ))
})

test_that("semantic timeline maps exact end_iter to tau", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  case <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_CASE.rds"
  )

  set <- te_article_case_set(list(case))
  d <- te_article_case_set_semantic_timeline(set)

  raw <- te_trace_data(case$trace)
  tau <- te_normalized_time(case$trace)

  m <- match(d$end_iter, raw$iter)

  expect_equal(
    d$tau,
    tau[m]
  )
})

test_that("Phi transition timeline preserves exact boundary iterations", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  case <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_CASE.rds"
  )

  set <- te_article_case_set(list(case))
  ev <- te_article_case_set_transition_timeline(set)

  if (nrow(ev)) {
    expect_true(all(
      ev$boundary_iter %in%
        te_trace_data(case$trace)$iter
    ))
    expect_true(all(is.finite(ev$tau)))
    expect_true(all(
      ev$semantics_origin ==
        "existing_article_pipeline"
    ))
  } else {
    succeed()
  }
})

test_that("regime occupancy fractions sum to one per semantic case", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(20)
    ),
    source_name = "E4A_B.rds"
  )

  set <- te_article_case_set(list(a, b))
  occ <- te_article_case_set_regime_occupancy(set)

  sums <- tapply(
    occ$fraction_windows,
    occ$case_key,
    sum
  )

  expect_true(all(abs(sums - 1) <= 1e-12))
})

test_that("transition signature remains descriptive", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  case <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_CASE.rds"
  )

  set <- te_article_case_set(list(case))
  sig <- te_article_case_set_transition_signature(set)

  if (nrow(sig)) {
    expect_true("transition_signature" %in% names(sig))
    expect_false(any(c(
      "rank",
      "winner",
      "score"
    ) %in% names(sig)))
  } else {
    succeed()
  }
})


test_that("semantic timeline obtains interval bounds from FormalSpec windows", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  case <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(18)
    ),
    source_name = "E1B_WINDOW_BOUNDS.rds"
  )

  set <- te_article_case_set(list(case))

  d <- te_article_case_set_semantic_timeline(set)

  win <- DSNeuralRNAS.FormalSpec::dsfs_windows_data(
    case$analysis$windows
  )
  cnd <- DSNeuralRNAS.FormalSpec::dsfs_regime_candidates_data(
    case$analysis$candidates
  )
  cnf <- DSNeuralRNAS.FormalSpec::dsfs_confirmed_regimes_data(
    case$analysis$confirmed
  )

  expect_equal(d$window_id, win$window_id)
  expect_equal(d$start_iter, win$start_iter)
  expect_equal(d$end_iter, win$end_iter)

  expect_equal(
    d$candidate_regime,
    cnd$candidate_regime[
      match(win$window_id, cnd$window_id)
    ]
  )

  expect_equal(
    d$confirmed_regime,
    cnf$confirmed_regime[
      match(win$window_id, cnf$window_id)
    ]
  )
})
