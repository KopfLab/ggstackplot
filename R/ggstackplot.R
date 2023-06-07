#' Stack a ggplot vertical
#'
#' `r lifecycle::badge('experimental')`
#' @importFrom prepared_data
#' @export
ggstackplot <- function(
    data, x, y, direction = c("guess", "horizontal", "vertical"),
    color = "black", overlap = 0, axis_size = 0.2, alternate_axes = TRUE,
    plot_template = ggplot() + geom_line() + geom_point() + theme_bw()) {


}

#' Intternal function to prepare the data for a ggstackplot
#' @param data the data frame to plot
#' @param x the x variable(s) to plot, accepts [dplyr::select()] syntax
#' @param y the y variable(s) to plot, accepts [dplyr::select()] syntax
#' @param direction whether to make a horizontal or vertical ggstackplot (default is for the function to get based on the number of x and y variables provided)
#' @param color which color to make the plots
#' @param overlap whether to overlap the plots
#' @param axis_size what fraction of a plot to allocate for the fixed axis
#' @param alternate_axes whether to alternate the sides on which the stacked axes are plotted
prepare_data <- function(
    data, x, y, direction = c("guess", "horizontal", "vertical"),
    color = "black", overlap = 0, axis_size = 0.2, alternate_axes = TRUE) {

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
        tidyr::pivot_longer(cols = dplyr::all_of(names(x)), names_to = ".var", values_to = ".x") |>
        dplyr::mutate(.y = !!sym(names(!!y)[1]))
    } else {
      data |>
        dplyr::rename(dplyr::all_of(x), dplyr::all_of(y)) |>
        tidyr::pivot_longer(cols = dplyr::all_of(names(y)), names_to = ".var", values_to = ".y") |>
        dplyr::mutate(.x = !!sym(names(!!x)[1]))
    }

  # prep config
  config <- dplyr::tibble(
    x = forcats::as_factor(names(x)),
    y = forcats::as_factor(names(y))
  )

  # do we have a valid length for color?
  if (!is.character(color) || !length(color) %in% c(1L, nrow(config))) {
    abort(sprintf("`color` must be either a single color or one for each variable (%d)", nrow(config)))
  }

  # do we have a valid overlap value?
  if (!is.numeric(overlap) || !all(overlap >= 0) || !all(overlap <= 1) || !length(overlap) %in% c(1L, nrow(config) - 1L)) {
    abort(sprintf("`overlap` must be either a single numeric value between 0 and 1 or one for each variable (%d)", nrow(config)))
  }

  # finish config
  config <- config |>
    dplyr::mutate()
      #   y = forcats::as_factor(y),
      #   color = color,
      #   overlap_bottom = c(overlap/2, NA),
      #   overlap_top = c(NA, overlap/2),
      #   axis_right = as.integer(y) %% 2L == 0L,
      #   first = as.integer(y) == 1L,
      #   last = as.integer(y) == length(levels(y))

  # tibble::tibble(
  #   y = forcats::as_factor(y),
  #   color = color,
  #   overlap_bottom = c(overlap/2, NA),
  #   overlap_top = c(NA, overlap/2),
  #   axis_right = as.integer(y) %% 2L == 0L,
  #   first = as.integer(y) == 1L,
  #   last = as.integer(y) == length(levels(y))
  # )


  return(config)
}

prepare_plots <- function() {

}

combine_plots <- function() {

}

