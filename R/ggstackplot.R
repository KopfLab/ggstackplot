#' Stack a ggplot vertical
#'
#' `r lifecycle::badge('experimental')`
#'
#' @export
ggstackplot <- function(data, x, y, direction = c("guess", "horizontal", "vertical")) {


}

prepare_data <- function(data, x, y, direction = c("guess", "horizontal", "vertical")) {
  # do we have a data frame?
  if (missing(data) || !is.data.frame(data)) {
    abort("`data` must be a data frame or tibble.")
  }

  # do x and y evaluate correctly?
  x <- try_fetch(
    tidyselect::eval_select(rlang::enexpr(x), data),
    error = function(cnd) {
      abort(
        "`x` must be a valid tidyselect expression.",
        parent = cnd
      )
    }
  )
  y <- try_fetch(
    tidyselect::eval_select(rlang::enexpr(y), data),
    error = function(cnd) {
      abort(
        "`y` must be a valid tidyselect expression.",
        parent = cnd
      )
    }
  )

  # do we have at least 1 x and 1 y?
  if (length(x) < 1 || length(y) < 1) {
    abort(c(
      "insufficient number of columns",
      "x" = if (length(x) < 1) "no `x` column selected",
      "x" = if (length(y) < 1) "no `y` column selected"
    ))
  }

  # do we have a valid direction picked?
  direction <- arg_match(direction)

  # are the direction and number of columns consistent
  if (direction == "horizontal" && length(y) > 1) {
    abort("`y` must be just 1 column for a horizontally stacked plot")
  } else if (direction == "vertical" && length(x) > 1) {
    abort("`x` must be just 1 column for a vertically stacked plot")
  } else if (direction == "guess" && length(x) > 1 && length(y) > 1) {
    abort("either `x` or `y` must be just 1 column for guessing the stacking direction")
  }

  # guess direction
  if (direction == "guess") direction <- if (length(x) > 1) "horizontal" else "vertical"

  # prep data (pivot based on direction)
  data_long <-
    if (direction == "horizontal") {
      data |>
        dplyr::rename(dplyr::all_of(x), dplyr::all_of(y)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(names(x))) |>
        dplyr::mutate(.x = .data$value, .y = !!sym(names(!!y)[1]))
    } else {
      data |>
        dplyr::rename(dplyr::all_of(x), dplyr::all_of(y)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(names(y))) |>
        dplyr::mutate(.y = .data$value, .x = !!sym(names(!!x)[1]))
    }

  return(data_long)
}

prepare_plots <- function() {

}

combine_plots <- function() {

}

# internal
make_plot_data <- function(df, x, y) {
  # for single x, multiple y!
  # stopifnot(length(x) == 1L)
  # stopifnot(length(y) > 1L)
  # df |>
  #   dplyr::select(x = dplyr::any_of(x), dplyr::all_of(y)) |>
  #   tidyr::pivot_longer(cols = dplyr::all_of(y)) |>
  #   dplyr::mutate(y = .data$name) |>
  #   tidyr::nest(data = -name)
}
