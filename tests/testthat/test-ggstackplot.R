context("ggstackplot")


test_that("prepare_data() tests", {
  # parameter checks
  expect_error(
    prepare_data(),
    "`data` must be a data frame or tibble"
  )
  expect_error(
    prepare_data(mtcars),
    "insufficient number of columns"
  )
  expect_error(
    prepare_data(mtcars, x = DNE),
    "`x` must be a valid tidyselect expression"
  )
  expect_error(
    prepare_data(mtcars, x = mpg),
    "insufficient number of columns"
  )
  expect_error(
    prepare_data(mtcars, x = mpg, y = DNE),
    "`y` must be a valid tidyselect expression"
  )
  expect_error(
    prepare_data(mtcars, y = mpg),
    "insufficient number of columns"
  )
  expect_error(
    prepare_data(mtcars, x = mpg, y = disp, direction = "DNE"),
    "`direction` must be one of"
  )
  expect_error(
    prepare_data(mtcars, x = mpg, y = c(disp, wt), direction = "horizontal"),
    "`y` must be just 1 column"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, direction = "vertical"),
    "`x` must be just 1 column"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = c(disp, qsec)),
    "either `x` or `y` must be just 1 column"
  )

})
