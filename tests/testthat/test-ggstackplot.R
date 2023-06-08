context("ggstackplot")


test_that("test create_stackplot_tibble() parameters", {
  # data
  expect_error(
    create_stackplot_tibble(),
    "`data` must be a data frame or tibble"
  )

  # x and y
  expect_error(
    create_stackplot_tibble(mtcars),
    "insufficient number of columns"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = DNE),
    "`x` must be a valid tidyselect expression"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = mpg),
    "insufficient number of columns"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = mpg, y = DNE),
    "`y` must be a valid tidyselect expression"
  )
  expect_error(
    create_stackplot_tibble(mtcars, y = mpg),
    "insufficient number of columns"
  )

  # direction
  expect_error(
    create_stackplot_tibble(mtcars, x = mpg, y = disp, direction = "DNE"),
    "`direction` must be one of"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = mpg, y = c(disp, wt), direction = "horizontal"),
    "`y` must be just 1 column"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, direction = "vertical"),
    "`x` must be just 1 column"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = c(disp, qsec)),
    "either `x` or `y` must be just 1 column"
  )

  # color
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, color = 42),
    "`color` must be either a single color or one for each variable"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, color = c("a", "b", "c")),
    "`color` must be either a single color or one for each variable"
  )

})


test_that("test create_stackplot_gtables() parameters", {

  # data
  expect_error(
    create_stackplot_gtables(),
    "`prepared_stackplot` must be a data frame or tibble"
  )
  expect_error(
    create_stackplot_gtables(data.frame(x = 5)),
    "with columns '.var'"
  )

  # prepared stackplot for testing
  expect_is(prep_sp <- prepare_stackplot(mtcars, "mpg", "wt"), "data.frame")

  # overlap
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = "42"),
    "`overlap` must be either a single numeric value.*between 0 and 1.*or one for each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = -0.01),
    "`overlap` must be either a single numeric value.*between 0 and 1.*or one for each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = 1.01),
    "`overlap` must be either a single numeric value.*between 0 and 1.*or one for each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = c(0.5, 0.5)),
    "`overlap` must be either a single numeric value.*between 0 and 1.*or one for each"
  )
})
