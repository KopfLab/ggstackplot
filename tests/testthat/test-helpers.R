context("helpers")

test_that("test get_plot_component_grobs()", {
  # parameters
  expect_error(get_plot_component_grobs(), "`gg` must be a ggplot object")
})

test_that("test calculate_axis_switch()", {

  # parameters
  expect_error(calculate_axis_switch(), "`var` must be")
  expect_error(calculate_axis_switch(1L), "`alternate` must be")
  expect_error(calculate_axis_switch(1L, TRUE), "`switch` must be")
  expect_error(calculate_axis_switch(1L, TRUE, TRUE), "`reverse` must be")

  # result
  expect_equal(calculate_axis_switch(1:4, FALSE, FALSE, FALSE), rep(FALSE, 4))
  expect_equal(calculate_axis_switch(1:4, FALSE, TRUE, FALSE), rep(TRUE, 4))
  expect_equal(calculate_axis_switch(1:4, TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(calculate_axis_switch(1:4, TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(calculate_axis_switch(1:4, TRUE, TRUE, FALSE), c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(calculate_axis_switch(1:5, TRUE, FALSE, FALSE), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(calculate_axis_switch(1:5, TRUE, FALSE, TRUE), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(calculate_axis_switch(1:5, TRUE, TRUE, TRUE), c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

