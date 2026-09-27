test_that("browser policy enables 3D when WebGL is supported", {
  p <- te_webgl_display_policy(TRUE)
  expect_equal(p$state, "webgl")
  expect_true(p$use_3d)
  expect_false(p$use_2d_fallback)
})

test_that("browser policy uses 2D fallback when WebGL is unavailable", {
  p <- te_webgl_display_policy(FALSE)
  expect_equal(p$state, "fallback_2d")
  expect_false(p$use_3d)
  expect_true(p$use_2d_fallback)
})

test_that("browser policy tolerates startup unknown state", {
  for (v in list(NULL, NA)) {
    p <- te_webgl_display_policy(v)
    expect_equal(p$state, "unknown")
    expect_false(p$use_3d)
    expect_false(p$use_2d_fallback)
  }
})
