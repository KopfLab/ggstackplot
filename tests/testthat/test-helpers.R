context("helpers")

test_that("test get_plot_component_grobs()", {

  # parameters
  expect_error(get_plot_component_grobs(), "`gg` must be a ggplot object")

})

