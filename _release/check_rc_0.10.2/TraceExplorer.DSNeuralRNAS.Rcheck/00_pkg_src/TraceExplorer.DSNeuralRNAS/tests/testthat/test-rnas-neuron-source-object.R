test_that("real rnas_neuron_train bridge supplies complete FormalSpec metadata", {
  skip_if_not_installed("DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  X <- matrix(
    c(
       0.5, -0.2,
       1.0,  0.3,
      -0.7,  0.8,
       0.0,  0.0
    ),
    ncol = 2,
    byrow = TRUE
  )
  y <- c(0.4, 0.8, -0.2, 0.1)

  fit <- DSNeuralRNAS::rnas_train_neuron(
    X = X,
    y = y,
    w0 = c(0.8, 0.3),
    b0 = 0.1,
    eta = 0.05,
    T = 20,
    activation = "tanh"
  )

  expect_s3_class(fit, "rnas_neuron_train")

  tr <- te_bridge(fit)

  expect_s3_class(tr, "te_trace")
  expect_equal(
    tr$provenance$source_class,
    "rnas_neuron_train"
  )
  expect_equal(
    tr$provenance$source_package,
    "DSNeuralRNAS"
  )
  expect_equal(
    tr$provenance$source_package_version,
    as.character(
      utils::packageVersion("DSNeuralRNAS")
    )
  )
  expect_false(
    tr$provenance$original_article_semantics_present
  )

  d <- te_trace_data(tr)
  expect_true(nrow(d) >= 2L)
  expect_true(all(c(
    "iter",
    "loss"
  ) %in% names(d)))
})

test_that("rnas_neuron_train source object imports as bridgeable article evidence", {
  skip_if_not_installed("DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  X <- matrix(
    c(
       0.5, -0.2,
       1.0,  0.3,
      -0.7,  0.8,
       0.0,  0.0
    ),
    ncol = 2,
    byrow = TRUE
  )
  y <- c(0.4, 0.8, -0.2, 0.1)

  fit <- DSNeuralRNAS::rnas_train_neuron(
    X = X,
    y = y,
    w0 = c(0.8, 0.3),
    b0 = 0.1,
    eta = 0.05,
    T = 20,
    activation = "tanh"
  )

  art <- te_article_import_object(
    fit,
    source_name = "E5A_DIRECT_NEURON_source.rds"
  )

  expect_s3_class(art, "te_article_case")
  expect_s3_class(art$trace, "te_trace")
  expect_null(art$analysis)
  expect_equal(
    art$classification$role[[1L]],
    "source_object"
  )

  st <- te_article_display_state(art)

  expect_equal(st$state, "bridgeable_source")
  expect_true(st$has_trace)
  expect_false(st$has_formalspec_analysis)
  expect_true(st$trace_views_enabled)
  expect_true(st$formalspec_views_enabled)
})

test_that("bridgeable rnas_neuron source can run frozen FormalSpec explicitly", {
  skip_if_not_installed("DSNeuralRNAS")
  skip_if_not_installed("DSNeuralRNAS.FormalSpec")

  X <- matrix(
    c(
       0.5, -0.2,
       1.0,  0.3,
      -0.7,  0.8,
       0.0,  0.0
    ),
    ncol = 2,
    byrow = TRUE
  )
  y <- c(0.4, 0.8, -0.2, 0.1)

  fit <- DSNeuralRNAS::rnas_train_neuron(
    X = X,
    y = y,
    w0 = c(0.8, 0.3),
    b0 = 0.1,
    eta = 0.05,
    T = 20,
    activation = "tanh"
  )

  art <- te_article_import_object(
    fit,
    source_name = "E5A_DIRECT_NEURON_source.rds"
  )

  fs <- te_run_formalspec(art$trace)

  expect_s3_class(fs, "te_formalspec_analysis")

  val <- te_validate_formalspec_analysis(fs)
  expect_true(all(val$status == "PASS"))

  s <- te_formalspec_summary(fs)
  expect_equal(
    s$freeze_id,
    "DSFS-1.0.0-FIRST-ARTICLE"
  )
  expect_false(s$calibration_on_trace)
})
