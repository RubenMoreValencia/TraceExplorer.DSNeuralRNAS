test_that("evidence bundle exports trace and existing FormalSpec semantics", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  fs <- te_run_formalspec(
    te_demo_mlp_trace(15)
  )
  art <- te_article_import_object(
    fs,
    source_name = "E1A_BASELINE.rds"
  )

  out <- tempfile("te-bundle-")

  result <- te_export_evidence_bundle(
    art,
    out
  )

  expect_true(dir.exists(result))

  required <- c(
    "canonical_trace.csv",
    "provenance.csv",
    "capabilities.csv",
    "space_availability.csv",
    "article_case_summary.csv",
    "windows.csv",
    "candidate_regimes.csv",
    "confirmed_regimes.csv",
    "boundary_events.csv",
    "formalspec_summary.csv",
    "manifest.csv",
    "traceexplorer_bundle.rds",
    "README.txt",
    "checksums.csv"
  )

  expect_true(all(
    required %in% list.files(result)
  ))

  checks <- utils::read.csv(
    file.path(result, "checksums.csv"),
    stringsAsFactors = FALSE
  )

  expect_true(nrow(checks) >= 1L)
  expect_true(all(nchar(checks$md5) == 32L))
})
