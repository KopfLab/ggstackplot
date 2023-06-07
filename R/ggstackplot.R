#' Stack a ggplot vertical
#'
#' `r lifecycle::badge('experimental')`
#' @param data the data frame to plot
#' @param x the x variable(s) to plot, accepts [dplyr::select()] syntax
#' @param y the y variable(s) to plot, accepts [dplyr::select()] syntax
#' @param direction whether to make a horizontal or vertical ggstackplot (default is for the function to get based on the number of x and y variables provided)
#' @param color which color to make the plots
#' @param overlap fractional overlap with next plot (by default no overlap), if providing multiple values, should be 1 shorter than the number of stacked plots
#' @param axis_size what fraction of a plot to allocate for the fixed axis
#' @param alternate_axes whether to alternate the sides on which the stacked axes are plotted
#' @param plot_template a template plot (ggplot object) to use for the stacked plots
#' @export
ggstackplot <- function(
    data, x, y, direction = c("guess", "horizontal", "vertical"),
    color = "black", overlap = 0, axis_size = 0.2, alternate_axes = TRUE,
    plot_template = ggplot() + geom_line() + geom_point() + theme_bw()) {

  # put everything together
  data |>
    prepare_data({{ x }}, {{ y }}, direction, color, overlap, axis_size, alternate_axes) |>
    prepare_plots(plot_template = plot_template) |>
    prepare_themes() |>
    combine_plots()
}

#' Internal function to prepare the data for a ggstackplot
#' @inheritParams ggstackplot
#' @return a nested data frame with each of stacked variables (.var), their plot configuration, and their data+
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
    .x = forcats::as_factor(names(x)),
    .y = forcats::as_factor(names(y))
  )

  # do we have a valid length for color?
  if (!is.character(color) || !length(color) %in% c(1L, nrow(config))) {
    abort(sprintf("`color` must be either a single color or one for each variable (%d)", nrow(config)))
  }

  # do we have a valid overlap value?
  if (!is.numeric(overlap) || !all(overlap >= 0) || !all(overlap <= 1) || !length(overlap) %in% c(1L, nrow(config) - 1L)) {
    abort(sprintf("`overlap` must be either a single numeric value between 0 and 1 or one for each variable (%d)", nrow(config)))
  }
  if(length(overlap) == 1L) overlap <- rep(overlap, times = nrow(config) - 1L)

  # finish config
  config <- config |>
    dplyr::mutate(
        .color = !!color,
        .overlap_bottom = c(!!overlap, NA),
        .overlap_top = c(NA, !!overlap),
        .axis_switch =
          if(!!alternate_axes && !!direction == "horizontal") {
            as.integer(.x) %% 2L == 0L
          } else if (!!alternate_axes && !!direction == "vertical") {
            as.integer(.y) %% 2L == 0L
          } else {
            FALSE
          },
        .first =
          (direction == "horizontal" & as.integer(.x) == 1L) |
          (direction == "vertical" & as.integer(.y) == 1L),
        .last =
          (direction == "horizontal" & as.integer(.x) == length(levels(.x))) |
          (direction == "vertical" & as.integer(.y) == length(levels(.y))),
        .var = if(direction == "horizontal") .x else .y,
        .direction = !!direction
    )

  # complete prepped data
  return(
    config |>
      tidyr::nest(config = -.data$.var) |>
      dplyr::left_join(
        tidyr::nest(data_long, data = -.data$.var),
        by = ".var"
      )
  )
}


#' Internal function to prepare plots to stack
#' @param prepared_data the prepared data output from [prepare_data()]
#' @inheritParams ggstackplot
#' @return the prepared tibble with a plot column added
prepare_plots <- function(prepared_data, plot_template) {

  # individual plot
  make_plot <- function(config, data) {
    plot_template %+%
      dplyr::cross_join(
        # join to get plotting config like color in there
        dplyr::select(config, -.data$.x, -.data$.y),
        data
      ) %+%
      aes(.x, .y, color = .color) +
      scale_color_identity() +
      scale_y_continuous(
        breaks = scales::pretty_breaks(5),
        # FIXME: add the dup axis to an existing y axis if there is already one in plot
        sec.axis = dup_axis()
      ) +
      # only do this if the common axis is not set yet!
      labs(x = config$.x, y = config$.y)
  }

  # compute plots
  prepared_data |>
    dplyr::mutate(
      plot = map2(config, data, make_plot)
    )

}

#' Internal function to prepare the themes for the stacked plots
#' @inheritParams prepare_plots
#' @return the prepared tibble with a theme column added
prepare_themes <- function(prepared_data) {

  # individual theme
  make_theme <- function(config) {
    theme(
      panel.grid = element_blank(),
      text = element_text(size = 16),
      axis.title.y.left = if(config$.axis_switch) element_blank() else element_text(color = config$.color),
      axis.text.y.left = if(config$.axis_switch) element_blank() else element_text(color = config$.color),
      axis.line.y.left = if(config$.axis_switch) element_blank() else element_line(color = config$.color),
      axis.ticks.y.left = if(config$.axis_switch) element_blank() else element_line(color = config$.color),
      axis.title.y.right = if(!config$.axis_switch) element_blank() else element_text(color = config$.color),
      axis.text.y.right = if(!config$.axis_switch) element_blank() else element_text(color = config$.color),
      axis.line.y.right = if(!config$.axis_switch) element_blank() else element_line(color = config$.color),
      axis.ticks.y.right = if(!config$.axis_switch) element_blank() else element_line(color = config$.color),
      axis.title.x = if(!config$.last) element_blank() else element_text(),
      axis.text.x = if(!config$.last) element_blank() else element_text(),
      axis.line.x = if(!config$.last) element_blank() else element_line(),
      axis.ticks.x = if(!config$.last) element_blank() else element_line(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(
        t = if (config$.first) 0 else -config$.overlap_top,
        b = if (config$.last) 0 else -config$.overlap_bottom,
        unit = "npc")
    )
  }

  # compute themes
  prepared_data |>
    dplyr::mutate(
      theme = map(config, make_theme)
    )
}

#' Internal function to combine the plots and themes into one big plot
#' @inheritParams prepare_plots
#' @return an object generated by [cowplot::plot_grid()]
combine_plots <- function(prepared_data) {

  # final plots <-
  plots <- prepared_data |>
    dplyr::mutate(
      final_plot = map2(plot, theme, ~.x + .y)
    ) |>
    dplyr::pull(final_plot)

  # combine it all
  cowplot::plot_grid(
    plotlist = plots,
    ncol = 1,
    align = "v",
    # the right number for the last plot depends on how tall you're saving the plot
    # this basically accounts for the additional x axis that's part of the last plot
    # fi this for x axis
    rel_heights = c(rep(1, times = length(plots) - 1), 1.2)
  )
}


