context("calculations")


test_that("test calculate_axis_switch()", {

  # parameters
  expect_error(calculate_axis_switch(), "`var` must be")
  expect_error(calculate_axis_switch(1L), "`alternate` must be")
  expect_error(calculate_axis_switch(1L, TRUE), "`switch` must be")

  # result
  expect_equal(calculate_axis_switch(1:5, FALSE, FALSE), rep(FALSE, 5))
  expect_equal(calculate_axis_switch(1:5, FALSE, TRUE), rep(TRUE, 5))
  expect_equal(calculate_axis_switch(1:5, TRUE, FALSE), c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(calculate_axis_switch(1:5, TRUE, TRUE), c(TRUE, FALSE, TRUE, FALSE, TRUE))
})

