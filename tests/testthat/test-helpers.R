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

test_that("test combine_plot_theme_add()", {
  # check that misleading axis modifications don't work
  expect_error(
    mtcars |>
    ggstackplot(
      x = mpg, y = c(wt, qsec, drat),
      add = list(wt = scale_x_continuous(limits = c(10, 15)))
    ),
    "invalid add-on.*shared x-axis"
  )
  # check that misleading axis modifications don't work
  expect_error(
    mtcars |>
      ggstackplot(
        y = mpg, x = c(wt, qsec, drat),
        add = list(wt = scale_y_continuous(limits = c(10, 15)))
      ),
    "invalid add-on.*shared y-axis"
  )
})

test_that("test process_add_ons()", {

  expect_error(process_add_ons(dplyr::tibble(), add = mean()), "`add` must be a list")
  expect_error(process_add_ons(dplyr::tibble(.var = c("a", "b", "c")), add = list("NDE" = list())), "no match for `add` component")
  expect_error(process_add_ons(dplyr::tibble(.var = c("a")), add = list(list(), list())), "`add` component.*index out of range")
  expect_error(process_add_ons(dplyr::tibble(.var = c("a")), add = list(list(), a = list())), "multiple `add` component")

})
