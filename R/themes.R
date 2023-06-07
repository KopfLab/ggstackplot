
# background ==========

# internal function to remove the background (i.e. make plots transparent)
# otherwise none of the plot overlapping works
theme_remove_background <- function() {
  # remove background (i.e. make everything transparent)
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )
}

# axes ===========

# internal function to modify a theme
theme_modify_axes <- function(
    change_x_color = NULL, change_y_color = NULL,
    remove_x_bottom = FALSE, remove_x_top = FALSE,
    remove_y_left = FALSE, remove_y_right = FALSE) {

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
   ) |> setNames(
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
      make_axis_theme_quos("x.bottom", remove_x_bottom, change_x_color),
      make_axis_theme_quos("x.top", remove_x_top, change_x_color),
      make_axis_theme_quos("y.left", remove_y_left, change_y_color),
      make_axis_theme_quos("y.right", remove_y_right, change_y_color)
    )

  # eval quos to get theme
  return(theme(!!!axis_modification_quos) |> quo() |> eval_tidy())

#
#   # bottom x axis
#   if (remove_x_bottom) {
#     theme <- theme + theme(
#       axis.title.x.bottom = element_blank(),
#       axis.text.x.bottom = element_blank(),
#       axis.line.x.bottom = element_blank(),
#       axis.ticks.x.bottom = element_blank()
#     )
#   } else if (!is.null(change_x_color)) {
#     theme <- theme + theme(
#       axis.title.x.bottom = element_text(color = change_x_color),
#       axis.text.x.bottom = element_text(color = change_x_color),
#       axis.line.x.bottom = element_line(color = change_x_color),
#       axis.ticks.x.bottom = element_line(color = change_x_color)
#     )
#   }
#
#   # top x axis
#   if (remove_x_top) {
#     theme <- theme + theme(
#       axis.title.x.top = element_blank(),
#       axis.text.x.top = element_blank(),
#       axis.line.x.top = element_blank(),
#       axis.ticks.x.top = element_blank()
#     )
#   } else if (!is.null(change_x_color)) {
#     theme <- theme + theme(
#       axis.title.x.top = element_text(color = change_x_color),
#       axis.text.x.top = element_text(color = change_x_color),
#       axis.line.x.top = element_line(color = change_x_color),
#       axis.ticks.x.top = element_line(color = change_x_color)
#     )
#   }
#
#   # left y axis
#   if (remove_y_left) {
#     theme <- theme + theme(
#       axis.title.y.left = element_blank(),
#       axis.text.y.left = element_blank(),
#       axis.line.y.left= element_blank(),
#       axis.ticks.y.left = element_blank()
#     )
#   } else if (!is.null(change_y_color)) {
#     theme <- theme + theme(
#       axis.title.y.left = element_text(color = change_y_color),
#       axis.text.y.left = element_text(color = change_y_color),
#       axis.line.y.left= element_line(color = change_y_color),
#       axis.ticks.y.left = element_line(color = change_y_color)
#     )
#   }
#
#   # right y axis
#   if (remove_y_right) {
#     theme <- theme + theme(
#       axis.title.y.right = element_blank(),
#       axis.text.y.right = element_blank(),
#       axis.line.y.right = element_blank(),
#       axis.ticks.y.right = element_blank()
#     )
#   } else if (!is.null(change_y_color)) {
#     theme <- theme + theme(
#       axis.title.y.right = element_text(color = change_y_color),
#       axis.text.y.right = element_text(color = change_y_color),
#       axis.line.y.right = element_line(color = change_y_color),
#       axis.ticks.y.right = element_line(color = change_y_color)
#     )
#   }

  return(theme)
}
