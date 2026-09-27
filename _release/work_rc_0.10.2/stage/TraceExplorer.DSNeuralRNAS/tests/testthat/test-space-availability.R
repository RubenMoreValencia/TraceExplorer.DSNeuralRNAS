test_that("didactic trace exposes all three 3D spaces", {
  tr <- te_demo_mlp_trace(10)
  av <- te_space_availability(tr)

  expect_equal(
    av$space,
    c("learning", "parameter", "control")
  )
  expect_true(all(av$available))
})

test_that("real rnas_mlp_train disables only parameter 3D space", {
  skip_if_not_installed("DSNeuralRNAS")

  src <- te_example_rnas_mlp(T = 20L, seed = 123L)
  tr <- te_bridge_rnas_mlp(src)
  av <- te_space_availability(tr)

  expect_true(
    av$available[av$space == "learning"]
  )
  expect_false(
    av$available[av$space == "parameter"]
  )
  expect_true(
    av$available[av$space == "control"]
  )

  expect_match(
    av$reason[av$space == "parameter"],
    "no inventa velocidad paramétrica"
  )
})

test_that("real ML.DSNeuralRNAS variable exposes all three spaces", {
  skip_if_not_installed("ML.DSNeuralRNAS")

  src <- te_example_ml_variable(iter = 20L, seed = 123L)
  tr <- te_bridge_ml_variable(src)
  av <- te_space_availability(tr)

  expect_true(all(av$available))
})

test_that("minimal trace does not invent unavailable 3D signals", {
  tr <- te_trace(
    data.frame(
      iter = 0:4,
      loss = c(1, .8, .6, .5, .4)
    ),
    provenance = list(
      source_class = "minimal-test"
    )
  )

  av <- te_space_availability(tr)

  expect_false(any(av$available))
})


test_that("space availability always returns exactly three rows", {
  tr <- te_demo_mlp_trace(10)
  a <- te_space_availability(tr)

  expect_equal(nrow(a), 3L)
  expect_equal(
    a$space,
    c("learning", "parameter", "control")
  )
  expect_equal(length(a$reason), 3L)
  expect_type(a$reason, "character")
})


test_that("space resolver keeps a valid requested space", {
  tr <- te_demo_mlp_trace(10)
  x <- te_resolve_space(tr, "parameter")

  expect_equal(x$space, "parameter")
  expect_false(x$fallback_used)
})

test_that("space resolver falls back when requested space is unavailable", {
  skip_if_not_installed("DSNeuralRNAS")

  src <- te_example_rnas_mlp(T = 15L, seed = 123L)
  tr <- te_bridge_rnas_mlp(src)

  x <- te_resolve_space(tr, "parameter")

  expect_equal(x$space, "learning")
  expect_true(x$fallback_used)
  expect_false(
    te_space_availability(tr)$available[
      te_space_availability(tr)$space == "parameter"
    ]
  )
})

test_that("space resolver uses first available space when request is NULL", {
  tr <- te_demo_mlp_trace(10)
  x <- te_resolve_space(tr, NULL)

  expect_equal(x$space, "learning")
  expect_true(x$fallback_used)
})
