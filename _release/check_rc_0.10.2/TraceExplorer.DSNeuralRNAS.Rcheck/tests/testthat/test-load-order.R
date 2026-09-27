test_that("default bridges are available after package load", {
  reg <- te_bridge_registry()

  expect_true(all(c(
    "data.frame",
    "dsfs_trace",
    "rnas_neuron_train",
    "rnas_control_eta_train",
    "rnas_mlp_train",
    "ml_variable_aprendida",
    "ml_componentes_dinamicos"
  ) %in% reg$source_class))
})

test_that("default bridge registration remains idempotent", {
  before <- te_bridge_registry()

  te_register_default_bridges()
  after <- te_bridge_registry()

  expect_equal(
    sort(unique(before$source_class)),
    sort(unique(after$source_class))
  )
})
