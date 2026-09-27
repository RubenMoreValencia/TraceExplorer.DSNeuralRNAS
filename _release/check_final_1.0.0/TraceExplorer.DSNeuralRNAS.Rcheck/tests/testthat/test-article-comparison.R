test_that("article case set preserves heterogeneous evidence roles", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  dynamic <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(12)
    ),
    source_name = "E1B_CASE.rds"
  )

  nondynamic <- te_article_import_object(
    structure(
      list(pass = TRUE),
      class = "dsfs_validation"
    ),
    source_name = "E2_VALIDATION.rds"
  )

  set <- te_article_case_set(
    list(dynamic, nondynamic),
    comparison_id = "mixed"
  )

  expect_s3_class(
    set,
    "te_article_case_set"
  )

  val <- te_validate_article_case_set(set)
  expect_true(all(val$status == "PASS"))

  idx <- te_article_case_set_index(set)

  expect_equal(nrow(idx), 2L)
  expect_true(any(
    idx$display_state == "dynamic_pipeline"
  ))
  expect_true(any(
    idx$display_state == "non_dynamic_evidence"
  ))
})

test_that("article comparison trace data keeps only dynamic traces without interpolation", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(10)
    ),
    source_name = "E1B_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(20)
    ),
    source_name = "E4A_B.rds"
  )

  set <- te_article_case_set(
    list(a, b)
  )

  d <- te_article_case_set_trace_data(set)

  expect_equal(
    nrow(d),
    nrow(te_trace_data(a$trace)) +
      nrow(te_trace_data(b$trace))
  )

  by_case <- split(
    d,
    d$case_key
  )

  expect_true(all(vapply(
    by_case,
    function(x) identical(
      as.numeric(range(x$tau)),
      c(0, 1)
    ),
    logical(1)
  )))
})

test_that("FormalSpec comparison reuses existing analyses only", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  dynamic <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(12)
    ),
    source_name = "E1B_CASE.rds"
  )

  trace_only <- structure(
    list(
      trace = te_demo_mlp_trace(12),
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
    list(dynamic, trace_only)
  )

  s <- te_article_case_set_formalspec_summary(set)

  expect_equal(nrow(s), 1L)
  expect_equal(
    s$semantics_origin,
    "existing_article_pipeline"
  )
  expect_false(
    any(s$case_id == "E5_SOURCE")
  )
})

test_that("catalog row loading is exact and does not apply role preference", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  root <- tempfile("article-compare-")
  dir.create(
    file.path(root, "objects", "E1", "E1B"),
    recursive = TRUE
  )

  fs1 <- te_run_formalspec(
    te_demo_mlp_trace(10)
  )
  fs2 <- te_run_formalspec(
    te_demo_mlp_trace(12)
  )

  p1 <- file.path(
    root,
    "objects",
    "E1",
    "E1B",
    "E1B_A.rds"
  )
  p2 <- file.path(
    root,
    "objects",
    "E1",
    "E1B",
    "E1B_B.rds"
  )

  saveRDS(fs1, p1)
  saveRDS(fs2, p2)

  catalog <- te_article_catalog(root)

  set <- te_article_load_case_set(
    catalog,
    rows = c(1L, 2L),
    comparison_id = "exact-rows"
  )

  expect_equal(length(set$cases), 2L)
  expect_equal(
    set$comparison_id,
    "exact-rows"
  )
})

test_that("article comparison bundle contains checksums and semantic policy", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  a <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(10)
    ),
    source_name = "E1B_A.rds"
  )
  b <- te_article_import_object(
    te_run_formalspec(
      te_demo_mlp_trace(12)
    ),
    source_name = "E4A_B.rds"
  )

  set <- te_article_case_set(
    list(a, b),
    comparison_id = "bundle-test"
  )

  out <- tempfile("article-comparison-bundle-")

  te_export_article_comparison_bundle(
    set,
    out
  )

  files <- list.files(out)

  expect_true(all(c(
    "comparison_index.csv",
    "comparison_trace_data.csv",
    "existing_formalspec_summary.csv",
    "boundary_events.csv",
    "manifest.csv",
    "article_case_set.rds",
    "checksums.csv"
  ) %in% files))

  manifest <- utils::read.csv(
    file.path(out, "manifest.csv"),
    stringsAsFactors = FALSE
  )

  expect_true(any(
    grepl(
      "Reuse existing article FormalSpec semantics only",
      manifest$value,
      fixed = TRUE
    )
  ))
})
