context("ggstackplot")


test_that("prepare_data() tests", {

  expect_error(prepare_data(), "`data` must be a data frame or tibble")
  expect_error(prepare_data(mtcars, x=dne), "`x` must be a valid tidyselect expression")
  expect_error(prepare_data(mtcars, x=mpg, y=dne), "`y` must be a valid tidyselect expression")

})
