test_that("didactic visual specification is more prominent", {
  student <- te_visual_spec("student")
  research <- te_visual_spec("research")

  expect_gt(student$title_size, research$title_size)
  expect_gt(student$marker_size, research$marker_size)
  expect_gt(student$gamma_size, research$gamma_size)
  expect_gt(student$phi_size, research$phi_size)
})

test_that("plotly axis specification preserves readable labels", {
  v <- te_visual_spec("student")
  a <- te_plotly_axis_spec("Tiempo normalizado tau", v, range = c(0, 1))

  expect_equal(a$title$text, "Tiempo normalizado tau")
  expect_equal(a$title$font$size, v$axis_title_size)
  expect_equal(a$tickfont$size, v$tick_size)
  expect_equal(a$range, c(0, 1))
})
