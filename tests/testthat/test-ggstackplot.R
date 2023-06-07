context("ggstackplot")


test_that("prepare_data() tests", {

  expect_error(prepare_data(), "`data` must be a data frame or tibble")

})
