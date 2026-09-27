test_that("five-level evidence contract is ordered and explicit", {
  c <- te_evidence_level_contract()

  expect_equal(nrow(c), 5L)
  expect_equal(c$level, 1:5)
  expect_equal(
    c$key,
    c(
      "observed_data",
      "final_result",
      "learning_trace",
      "formalspec_evidence",
      "analytical_reference"
    )
  )
})

test_that("E1 article case receives explicit analytical reference", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  art <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(18)),
    source_name = "E1B_050.rds"
  )
  art$experiment <- "E1"
  art$stage <- "E1B"
  art$case_id <- "E1B_050"

  ref <- te_article_analytical_reference(art)

  expect_s3_class(ref, "te_analytical_reference")
  expect_equal(ref$expected_value, 0.5)
  expect_equal(
    ref$type,
    "quadratic_stability_boundary"
  )
  expect_true(all(
    te_validate_analytical_reference(ref)$status ==
      "PASS"
  ))
})

test_that("non-E1 article case does not receive invented analytical reference", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  art <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(12)),
    source_name = "E5B_STD2_pipeline.rds"
  )
  art$experiment <- "E5"
  art$stage <- "E5B"
  art$case_id <- "E5B_STD2"

  expect_null(
    te_article_analytical_reference(art)
  )
})

test_that("dynamic article pipeline builds a valid five-level stack", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  art <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(18)),
    source_name = "E1B_050.rds"
  )
  art$experiment <- "E1"
  art$stage <- "E1B"
  art$case_id <- "E1B_050"

  stack <- te_evidence_stack(art)

  expect_s3_class(stack, "te_evidence_stack")
  expect_true(all(
    te_validate_evidence_stack(stack)$status ==
      "PASS"
  ))

  levels <- te_evidence_stack_levels(stack)

  expect_equal(
    levels$availability[
      levels$key == "learning_trace"
    ],
    "available"
  )

  expect_equal(
    levels$availability[
      levels$key == "formalspec_evidence"
    ],
    "available_existing"
  )

  expect_equal(
    levels$availability[
      levels$key == "analytical_reference"
    ],
    "available"
  )
})

test_that("non-dynamic article evidence does not invent trace or FormalSpec", {
  obj <- structure(
    list(pass = TRUE),
    class = "dsfs_validation"
  )

  art <- te_article_import_object(
    obj,
    source_name = "E5D_FNL_SCOPE_source.rds"
  )
  art$experiment <- "E5"
  art$stage <- "E5D"
  art$case_id <- "E5D_FNL_SCOPE"

  stack <- te_evidence_stack(art)
  levels <- te_evidence_stack_levels(stack)

  expect_equal(
    levels$availability[
      levels$key == "learning_trace"
    ],
    "unavailable"
  )
  expect_equal(
    levels$availability[
      levels$key == "formalspec_evidence"
    ],
    "unavailable"
  )
  expect_equal(
    levels$availability[
      levels$key == "analytical_reference"
    ],
    "not_applicable"
  )
})

test_that("implementation decisions preserve epistemic guardrails", {
  tr <- te_demo_mlp_trace(12)
  stack <- te_evidence_stack(tr)

  d <- te_evidence_stack_decisions(stack)

  expect_equal(
    d$status[d$decision == "infer_missing_trace"],
    "PROHIBITED"
  )
  expect_equal(
    d$status[d$decision == "automatic_learning_policy"],
    "OUT_OF_SCOPE"
  )
})

test_that("E1 analytical contrast is descriptive and exact", {
  tr <- te_trace(
    data.frame(
      iter = 0:4,
      loss = c(2.5, 2, 1.5, 2.2, 4),
      grad_norm = c(4, 3, 2, 3, 5),
      eta = c(0.05, 0.05, 0.5, 0.6, 0.6)
    ),
    provenance = list(
      dataset_id = "quadratic-reference"
    )
  )

  ref <- te_analytical_reference(
    type = "quadratic_stability_boundary",
    expression = "eta_star = 2 / lambda_max(A)",
    expected_value = 0.5,
    parameters = list(lambda_max_A = 4),
    scope = list(experiment = "E1")
  )

  stack <- te_evidence_stack(
    tr,
    analytical_reference = ref
  )

  c <- te_evidence_analytical_contrast(stack)

  expect_equal(
    c$value[c$metric == "eta_star"],
    0.5
  )
  expect_equal(
    c$value[c$metric == "n_above"],
    2
  )
  expect_equal(
    c$value[c$metric == "first_above_iter"],
    3
  )
})

test_that("extension points separate evidence observation from future control", {
  e <- te_evidence_extension_points()

  expect_true(
    "decision_policy_provider" %in%
      e$extension_point
  )
  expect_true(any(
    grepl(
      "not implemented as control",
      e$current_dsneural_role,
      fixed = TRUE
    )
  ))
})
