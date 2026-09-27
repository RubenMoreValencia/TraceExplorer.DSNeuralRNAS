test_that("article inventory distinguishes dynamic and non-dynamic objects", {
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  root <- tempfile("article-project-")
  dir.create(
    file.path(root, "objects", "E1", "E1A"),
    recursive = TRUE
  )
  dir.create(
    file.path(root, "objects", "E3", "E3D"),
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
      "E1A_TEST.rds"
    )
  )

  saveRDS(
    structure(
      list(valid = FALSE),
      class = "dsfs_validation"
    ),
    file.path(
      root,
      "objects",
      "E3",
      "E3D",
      "E3D_TEST.rds"
    )
  )

  inv <- te_article_inventory(root)

  expect_equal(nrow(inv), 2L)
  expect_true(any(inv$role == "dynamic_pipeline"))
  expect_true(any(inv$role == "summary_or_validation"))
  expect_true(all(nzchar(inv$source_md5)))
})
