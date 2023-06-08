#' Recommended base theme for stacked gg plots
#'
#'
#'
#' @return `ggplot2::theme()` object
#' @examples
#' library(ggplot2)
#' template <- ggplot() + geom_line() + theme_stackplot()
#'
#' ggstackplot(
#'   data = mtcars,
#'   x = mpg, y = c(wt, qsec, drat),
#'   color = c("#E41A1C", "#377EB8", "#4DAF4A"),
#'   template = template
#' )
#'
#' @export
theme_stackplot <- function() {
  theme_bw() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank()
    )
}

# make color axis theme ======

# internal function to generate basic color axis theme
make_color_axis_theme <- function(config) {
  # make the variable axis colorful
  theme_modify_axes(
    axis = if (config$.direction == "horizontal") "x" else "y",
    change_color = if (!is.na(config$.color)) config$.color else NULL,
    remove_primary = !is.na(config$.axis_switch) && config$.axis_switch,
    remove_secondary = !is.na(config$.axis_switch) && !config$.axis_switch
  ) +
    # make the shared axis uniform
    theme_modify_axes(
      axis = if (config$.direction == "horizontal") "y" else "x"
    )
}

# internal function to modify the axes for a theme
theme_modify_axes <- function(axis = c("x", "y"), change_color = NULL, remove_primary = FALSE, remove_secondary = FALSE) {

  # safety checks
  axis <- arg_match(axis)
  primary <- if(axis == "x") "x.bottom" else "y.left"
  secondary <- if(axis == "x") "x.top" else "y.right"

  # make axis config
  make_axis_theme_quos <- function(axis, blank, color) {
    list(
      # set axis.title
      if (blank) quo(element_blank())
      else if (!is.null(color)) quo(element_text(color = !!color))
      else quo(element_text()),
      # set axis text
      if (blank) quo(element_blank())
      else if (!is.null(color)) quo(element_text(color = !!color))
      else quo(element_text()),
      # set axis line
      if (blank) quo(element_blank())
      else if (!is.null(color)) quo(element_line(color = !!color))
      else quo(element_line()),
      # set axis ticks
      if (blank) quo(element_blank())
      else if (!is.null(color)) quo(element_line(color = !!color))
      else quo(element_line())
    ) |> stats::setNames(
      paste0(
        c("axis.title.",
          "axis.text.",
          "axis.line.",
          "axis.ticks."),
        axis)
    )
  }

  # assemble axis config
  axis_modification_quos <-
    c(
      make_axis_theme_quos(primary, remove_primary, change_color),
      make_axis_theme_quos(secondary, remove_secondary, change_color)
    )

  # eval quos to get theme
  return(theme(!!!axis_modification_quos) |> quo() |> eval_tidy())
}


# make base plot =========

# internal function to make individual plot from base plot, config and data
make_plot <- function(config, data, template) {
  # add data and x/y aesthetics
  plot <-
    template %+%
    dplyr::cross_join(config, data) %+%
    aes(.data$.x, .data$.y)

  # add color and fill aesthetics if set
  if (!is.na(config$.color))
    plot <- plot %+% aes(color = .data$.color, fill = .data$.color) +
      scale_color_identity() +
      scale_fill_identity()

  # set the x axis
  if (!plot$scales$has_scale("x")) {
    # none yet
    plot <- plot + scale_x_continuous(
      name = config$.xvar,
      sec.axis = if(config$.direction == "horizontal") dup_axis() else waiver()
    )
  } else {
    # has one, see if it needs edits
    if (config$.direction == "horizontal" && is(plot$scales$scales[[which(plot$scales$find("x"))[1]]]$secondary.axis, "waiver")) {
      # add secondary axis
      plot$scales$scales[[which(plot$scales$find("x"))[1]]]$secondary.axis <- dup_axis()
    }
    if (methods::is(plot$scales$scales[[which(plot$scales$find("x"))[1]]]$name, "waiver")) {
      # add name
      plot$scales$scales[[which(plot$scales$find("x"))[1]]]$name <- config$.xvar
    }
  }

  # set the y axis
  if (!plot$scales$has_scale("y")) {
    # none yet
    plot <- plot + scale_y_continuous(
      name = config$.yvar,
      sec.axis = if(config$.direction == "vertical") dup_axis() else waiver()
    )
  } else {
    # has one, see if it needs edits
    if (config$.direction == "vertical" && is(plot$scales$scales[[which(plot$scales$find("y"))[1]]]$secondary.axis, "waiver")) {
      # add secondary axis
      plot$scales$scales[[which(plot$scales$find("y"))[1]]]$secondary.axis <- dup_axis()
    }
    if (methods::is(plot$scales$scales[[which(plot$scales$find("y"))[1]]]$name, "waiver")) {
      # add name
      plot$scales$scales[[which(plot$scales$find("y"))[1]]]$name <- config$.yvar
    }
  }

  return(plot)
}

# combine plot and theme =============

# internal function to combine plots with themes considering whether shared axis should be simplified
combine_plot_and_theme <- function(prepared_stackplot, simplify_shared_axis) {
  # safety checks
  stopifnot(
    "`simplify_shared_axis` must be TRUE or FALSE" = !missing(simplify_shared_axis) && is_bool(simplify_shared_axis)
  )

  # which axis to simplify (if any)?
  simplify_axis <-
    if(simplify_shared_axis && prepared_stackplot$config[[1]]$.direction == "horizontal") "y"
  else if(simplify_shared_axis && prepared_stackplot$config[[1]]$.direction == "vertical") "x"
  else NULL

  # combine plot and theme
  prepared_stackplot <-
    prepared_stackplot |>
    dplyr::mutate(
      plot_w_theme = map2(.data$plot, .data$theme, ~{
        plot <- .x + .y
        if(!is.null(simplify_axis))
          plot <- plot + theme_modify_axes(simplify_axis, remove_primary = TRUE, remove_secondary = TRUE)
        return(plot)
      })
    )

  # return the augmented tibble with the new plot_w_theme column
  return(prepared_stackplot)
}

# gtable functions ======

# helper function to get plot component grobs
get_plot_component_grobs <- function(gg, keep_filter = TRUE, delete_filter = FALSE) {
  stopifnot("`gg` must be a ggplot object" = !missing(gg) && methods::is(gg, "ggplot"))

  # get grobs table and layout
  gg_table <- ggplot2::ggplotGrob(gg)
  gg_layout <- gg_table$layout |>
    dplyr::mutate(
      keep = {{ keep_filter }},
      delete = !.data$keep | {{ delete_filter }}
    )

  # update grobs table
  gg_table$grobs[gg_layout$delete] <- NULL
  gg_table$layout <- dplyr::filter(gg_layout, !.data$delete)

  # return
  return(gg_table)
}

# check if a set of grobs is zero
is_zero_grob <- function(gg_table) {
  map_lgl(gg_table$grob, methods::is, "zeroGrob")
}

# internal function to align gtables in a data frame with columns gtable and .direction
align_gtables <- function(gtables) {
  gtables |>
    dplyr::mutate(
      gtable = cowplot::align_plots(
        plotlist = .data$gtable,
        align = if(.direction[1] == "horizontal") "h" else "v",
        axis = if(.direction[1] == "horizontal") "tb" else "lr"
      )
    )
}

# internal function to combine gtables from a data frame with columns gtable, x, y, width, heights
# note: make sure the gtable are already aligned the way you want them aligned
combine_gtables <- function(gtables) {
  p <- cowplot::ggdraw()
  for (i in 1:nrow(gtables)) {
    p <- p + with(gtables[i,], cowplot::draw_grob(gtable[[1]], x, y, width, height, valign = 1))
  }
  return(p)
}
