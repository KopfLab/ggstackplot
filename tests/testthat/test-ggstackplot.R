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
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = c(disp, drat)),
    "too many columns"
  )

  # remove_na and other bools
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, remove_na = 42),
    "`remove_na` must be TRUE or FALSE"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, both_axes = 42),
    "`both_axes` must be TRUE or FALSE"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, alternate_axes = 42),
    "`alternate_axes` must be TRUE or FALSE"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, switch_axes = 42),
    "`switch_axes` must be TRUE or FALSE"
  )

  # color / palette
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, color = 42, palette = 42),
    "can only set either"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, color = 42),
    "`color` must be either a single color or one for each variable"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, color = c("a", "b", "c")),
    "`color` must be either a single color or one for each variable"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, palette = 42),
    "must be a string"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, palette = c("a", "b")),
    "must be a string"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, wt), y = disp, palette = "DNE"),
    "must be.*identifying a valid RColorBrewer palette"
  )
  expect_error(
    create_stackplot_tibble(mtcars, x = c(mpg, cyl, hp, drat, wt, qsec, vs, am, gear), y = disp, palette = "Accent"),
    "must be.*identifying a valid RColorBrewer palette"
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
