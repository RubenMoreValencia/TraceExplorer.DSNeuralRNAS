test_that("article catalog adds purpose and display state", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  root <- tempfile("article-nav-")
  dir.create(
    file.path(root, "objects", "E1", "E1A"),
    recursive = TRUE
  )
  dir.create(
    file.path(root, "objects", "E2", "E2A"),
    recursive = TRUE
  )

  fs <- te_run_formalspec(
    te_demo_mlp_trace(12)
  )

  saveRDS(
    fs,
    file.path(
      root,
      "objects",
      "E1",
      "E1A",
      "E1A_DYNAMIC.rds"
    )
  )

  saveRDS(
    structure(
      list(pass = TRUE),
      class = "dsfs_validation"
    ),
    file.path(
      root,
      "objects",
      "E2",
      "E2A",
      "E2A_SUMMARY.rds"
    )
  )

  cat <- te_article_catalog(root)

  expect_equal(nrow(cat), 2L)
  expect_true("experiment_purpose" %in% names(cat))
  expect_true("display_state" %in% names(cat))
  expect_true(any(cat$display_state == "dynamic_pipeline"))
  expect_true(any(cat$display_state == "non_dynamic_evidence"))
})

test_that("article filter preserves explicit role and experiment selection", {
  catalog <- data.frame(
    experiment = c("E1", "E1", "E2"),
    stage = c("E1A", "E1B", "E2A"),
    case_id = c("A", "B", "C"),
    role = c(
      "dynamic_pipeline",
      "summary_or_validation",
      "dynamic_pipeline"
    ),
    dynamic_eligible = c(TRUE, FALSE, TRUE),
    bridge_eligible = c(FALSE, FALSE, FALSE),
    path = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  out <- te_article_filter(
    catalog,
    experiment = "E1",
    role = "dynamic_pipeline"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$case_id, "A")
})

test_that("catalog summary counts evidence by experiment and role", {
  catalog <- data.frame(
    experiment = c("E1", "E1", "E2"),
    role = c(
      "dynamic_pipeline",
      "dynamic_pipeline",
      "summary_or_validation"
    ),
    stringsAsFactors = FALSE
  )

  s <- te_article_catalog_summary(catalog)

  expect_equal(sum(s$n_objects), 3L)
  expect_equal(
    s$n_objects[
      s$experiment == "E1" &
        s$role == "dynamic_pipeline"
    ],
    2L
  )
})

test_that("catalog loader prefers dynamic pipeline for duplicate case", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  root <- tempfile("article-duplicate-")
  dir.create(
    file.path(root, "objects", "E5", "E5A"),
    recursive = TRUE
  )

  fs <- te_run_formalspec(
    te_demo_mlp_trace(12)
  )

  pipeline_path <- file.path(
    root,
    "objects",
    "E5",
    "E5A",
    "E5A_CASE_pipeline.rds"
  )
  summary_path <- file.path(
    root,
    "objects",
    "E5",
    "E5A",
    "E5A_CASE.rds"
  )

  saveRDS(fs, pipeline_path)
  saveRDS(
    structure(
      list(pass = TRUE),
      class = "dsfs_validation"
    ),
    summary_path
  )

  cat <- te_article_catalog(root)

  # Normalize the summary row to the same logical case to exercise priority.
  cat$case_id[
    basename(cat$path) == "E5A_CASE.rds"
  ] <- "E5A_CASE"

  art <- te_article_load_catalog_case(
    cat,
    case_id = "E5A_CASE",
    experiment = "E5"
  )

  expect_equal(
    art$classification$role[[1L]],
    "dynamic_pipeline"
  )
  expect_true(
    art$uses_existing_article_semantics
  )
})

test_that("dynamic article cases can enter multirun without pooling semantics", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(10)),
    source_name = "E1A_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(te_demo_mlp_trace(12)),
    source_name = "E4A_B.rds"
  )

  mr <- te_article_cases_multirun(
    list(a, b)
  )

  expect_s3_class(mr, "te_multirun")
  expect_equal(length(mr$traces), 2L)
  expect_true(all(
    te_validate_multirun(mr)$status == "PASS"
  ))
})
