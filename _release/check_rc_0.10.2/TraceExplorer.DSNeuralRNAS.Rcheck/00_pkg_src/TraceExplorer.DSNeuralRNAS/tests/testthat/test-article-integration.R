test_that("existing FormalSpec analysis imports as article evidence", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  tr <- te_demo_mlp_trace(20)
  fs <- te_run_formalspec(tr)

  art <- te_article_import_object(
    fs,
    source_name = "E1A_BASELINE.rds"
  )

  expect_s3_class(art, "te_article_case")
  expect_s3_class(art$trace, "te_trace")
  expect_s3_class(
    art$analysis,
    "te_formalspec_analysis"
  )
  expect_true(
    art$uses_existing_article_semantics
  )

  expect_equal(
    te_trace_data(art$trace)$loss,
    DSNeuralRNAS.FormalSpec::dsfs_trace_data(
      fs$trace
    )$loss
  )

  s <- te_article_case_summary(art)
  expect_true(s$dynamic_eligible)
  expect_equal(s$n_states, 21L)
})

test_that("E3-style result wrapper exposes its pipeline", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  fs <- te_run_formalspec(
    te_demo_mlp_trace(15)
  )

  obj <- list(
    pipeline = fs,
    summary = data.frame(
      scenario_id = "E3C_TEST",
      stringsAsFactors = FALSE
    )
  )

  art <- te_article_import_object(
    obj,
    source_name = "E3C_TEST.rds"
  )

  expect_equal(art$case_id, "E3C_TEST")
  expect_true(
    art$uses_existing_article_semantics
  )
})

test_that("summary-only object is not turned into a pseudo-trace", {
  obj <- structure(
    list(
      metric = 1,
      pass = TRUE
    ),
    class = "dsfs_validation"
  )

  art <- te_article_import_object(
    obj,
    source_name = "E3D_wrong_scale_validation.rds"
  )

  expect_s3_class(art, "te_article_case")
  expect_null(art$trace)
  expect_null(art$analysis)
  expect_false(
    art$classification$dynamic_eligible[[1L]]
  )
})

test_that("article RDS loader records source checksum", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  fs <- te_run_formalspec(
    te_demo_mlp_trace(12)
  )

  path <- tempfile(fileext = ".rds")
  saveRDS(fs, path)

  art <- te_article_load_rds(
    path,
    source_name = "E4A_TEST.rds"
  )

  expect_true(nzchar(art$source_md5))
  expect_equal(
    art$source_md5,
    unname(tools::md5sum(path)[[1L]])
  )
})


test_that("non-dynamic article evidence has a safe display state", {
  obj <- structure(
    list(
      metric = 1,
      pass = TRUE
    ),
    class = "dsfs_validation"
  )

  art <- te_article_import_object(
    obj,
    source_name = "E2_validation.rds"
  )

  st <- te_article_display_state(art)

  expect_equal(st$state, "non_dynamic_evidence")
  expect_false(st$has_trace)
  expect_false(st$trace_views_enabled)
  expect_false(st$formalspec_views_enabled)
})

test_that("dynamic article pipeline enables trace and FormalSpec views", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  fs <- te_run_formalspec(
    te_demo_mlp_trace(12)
  )

  art <- te_article_import_object(
    fs,
    source_name = "E1A_BASELINE.rds"
  )

  st <- te_article_display_state(art)

  expect_equal(st$state, "dynamic_pipeline")
  expect_true(st$has_trace)
  expect_true(st$has_formalspec_analysis)
  expect_true(st$trace_views_enabled)
  expect_true(st$formalspec_views_enabled)
})
