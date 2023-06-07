context("ggstackplot")


test_that("test prepare_data() parameters", {

  # data
  expect_error(
    prepare_data(),
    "`data` must be a data frame or tibble"
  )

  # x and y
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

  # direction
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

  # color
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, color = 42),
    "`color` must be either a single color or one for each variable"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, color = c("a", "b", "c")),
    "`color` must be either a single color or one for each variable"
  )

  # overlap
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, overlap = "42"),
    "`overlap` must be either a single numeric value between 0 and 1 or one for each variable"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, overlap = -0.01),
    "`overlap` must be either a single numeric value between 0 and 1 or one for each variable"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt), y = disp, overlap = 1.01),
    "`overlap` must be either a single numeric value between 0 and 1 or one for each variable"
  )
  expect_error(
    prepare_data(mtcars, x = c(mpg, wt, vs), y = disp, overlap = c(0.5, 0.5, 0.5)),
    "`overlap` must be either a single numeric value between 0 and 1 or one for each variable"
  )



})
