test_that("test create_stackplot_tibble() safety checks", {
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

test_that("test create_stackplot_gtables() safety checks", {

  # data
  expect_error(
    create_stackplot_gtables(),
    "`prepared_stackplot` must be a data frame or tibble"
  )
  expect_error(
    create_stackplot_gtables(data.frame(x = 5)),
    "with columns.*.var"
  )

  # prepared stackplot for testing
  expect_s3_class(prep_sp <- prepare_stackplot(mtcars, "mpg", "wt"), "data.frame")

  # overlap
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = "42"),
    "`overlap` must be either a single numeric value.*between 0 and 1.*one for.*each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = -0.01),
    "`overlap` must be either a single numeric value.*between 0 and 1.*one for.*each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = 1.01),
    "`overlap` must be either a single numeric value.*between 0 and 1.*one for.*each"
  )
  expect_error(
    create_stackplot_gtables(prep_sp, overlap = c(0.5, 0.5)),
    "`overlap` must be either a single numeric value.*between 0 and 1.*one for.*each"
  )
  expect_warning(
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat), add = list(qsec = DNE)),
    "failed to parse added code"
  )
})

test_that("test ggstackplot() functionality", {
  # this indirectly tests all the other functionsˇ

  # `x` and `y` arguments
  vdiffr::expect_doppelganger(
    "vertically stacked plot",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec, drat)))
  vdiffr::expect_doppelganger(
    "horizontally stacked plot",
    ggstackplot(mtcars, y = mpg, x = c(wt, qsec, drat))
  )
  vdiffr::expect_doppelganger(
    "plot with color palette",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec), palette = "Set1")
  )
  vdiffr::expect_doppelganger(
    "plot with manual color definition",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec), color = c("#E41A1C", "#377EB8"))
  )
  vdiffr::expect_doppelganger(
    "plot with duplicated axes",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec), both_axes = TRUE)
  )
  vdiffr::expect_doppelganger(
    "plot without alternating axes",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec), alternate_axes = FALSE)
  )
  vdiffr::expect_doppelganger(
    "plot with switched axes",
    ggstackplot(mtcars, x = mpg, y = c(wt, qsec), switch_axes = TRUE)
  )
  vdiffr::expect_doppelganger(
    "plot with overlap",
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat), overlap = 1)
  )
  vdiffr::expect_doppelganger(
    "plot with large shared axis space",
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat), shared_axis_size = 0.5)
  )
  vdiffr::expect_doppelganger(
    "plot with custom template",
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat),
                template = ggplot() + geom_line())
  )
  vdiffr::expect_doppelganger(
    "plot with template sec axis labs",
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat),
                alternate_axes = FALSE, both_axes = TRUE,
                template = ggplot() + geom_line() +
                  scale_x_continuous(sec.axis = dup_axis())+
                  labs(x = "x", y = "y"))
  )
  vdiffr::expect_doppelganger(
    "vstack w axis defs in template",
    ggstackplot(
      mtcars, x = mpg, y = c(qsec, drat),
      template =
        ggplot() + geom_line() +
        scale_x_log10() +
        scale_y_continuous()
    )
  )
  vdiffr::expect_doppelganger(
    "vstack w axis defs and labs in template",
    ggstackplot(
      mtcars, x = mpg, y = c(qsec, drat),
      template =
        ggplot() + geom_line() +
        scale_x_continuous() +
        scale_y_continuous() +
        labs(x = "x", y = "y")
    )
  )
  vdiffr::expect_doppelganger(
    "hstack w axis defs in template",
    ggstackplot(
      mtcars, y = mpg, x = c(qsec, drat),
      template =
        ggplot() + geom_line() +
        scale_x_continuous() +
        scale_y_continuous()
    )
  )
  vdiffr::expect_doppelganger(
    "hstack w axis defs and labs in template",
    ggstackplot(
      mtcars, y = mpg, x = c(qsec, drat),
      template =
        ggplot() + geom_line() +
        scale_x_continuous() +
        scale_y_continuous() +
        labs(x = "x", y = "y")
    )
  )
  vdiffr::expect_doppelganger(
    "plot with added elements",
    ggstackplot(mtcars, x = mpg, y = c(qsec, drat), add = list(qsec = geom_path()))
  )
  # advanced
  expect_snapshot({
    plot_prep <- mtcars |> prepare_stackplot(x = mpg, y = c(wt, qsec), palette = "Set1")
  })
  expect_snapshot({
    plot_prep$plot[[2]] <- ggplot(mtcars) + aes(mpg, drat) + geom_point()
    plot_prep$theme[[2]] <- theme_bw()
    plot_prep
  })
  vdiffr::expect_doppelganger("assembled plot", plot_prep |> assemble_stackplot())
  # debug output
  expect_snapshot(
    x <- ggstackplot(mtcars, x = mpg, y = c(qsec, drat), debug = TRUE)
  )
})

