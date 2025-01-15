# main theme ========

#' Recommended base theme for stacked gg plots
#'
#' Returns a basic ggplot2 theme that extends [ggplot2::theme_bw()] with a transparent plot background to make sure overlapping plots do not cover each other up.
#'
#' @return [ggplot2::theme()] object
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
    remove_primary = !is.na(config$.axis_switch) &&
      ((config$.direction == "horizontal" && !config$.axis_switch) ||
      (config$.direction == "vertical" && config$.axis_switch)),
    remove_secondary = !is.na(config$.axis_switch) &&
      ((config$.direction == "horizontal" && config$.axis_switch) ||
         (config$.direction == "vertical" && !config$.axis_switch))
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
  limits <- c(config$.shared_axis_min, config$.shared_axis_max)
  if (!plot$scales$has_scale("x")) {
    # none yet
    plot <- plot + scale_x_continuous(
      sec.axis = if(config$.direction == "horizontal") dup_axis() else waiver(),
      limits = if(config$.direction == "vertical") limits else NULL
    )
    # check if template has x labels
    if (is.null(template$labels$x))
      plot <- plot + labs(x = config$.xvar)
    else
      plot <- plot + labs(x = template$labels$x)
  } else {
    # has one, see if it needs edits
    x_scale_idx <- which(plot$scales$find("x"))[1]
    if (config$.direction == "horizontal" && methods::is(plot$scales$scales[[x_scale_idx]]$secondary.axis, "waiver")) {
      # add secondary axis
      plot$scales$scales[[x_scale_idx]]$secondary.axis <- dup_axis()
    }
    if (methods::is(plot$scales$scales[[x_scale_idx]]$name, "waiver")) {
      # axis but no name - check if template has x labels
      if (is.null(template$labels$x))
        plot <- plot + labs(x = config$.xvar)
      else
        plot <- plot + labs(x = template$labels$x)
    }
    if (config$.direction == "vertical" && is.null(plot$scales$scales[[x_scale_idx]]$limits)) {
      # add limits
      plot$scales$scales[[x_scale_idx]]$limits <-
        sort(plot$scales$scales[[x_scale_idx]]$transform(limits))
    }
  }

  # set the y axis
  if (!plot$scales$has_scale("y")) {
    # none yet
    plot <- plot + scale_y_continuous(
      sec.axis = if(config$.direction == "vertical") dup_axis() else waiver(),
      limits = if(config$.direction == "horizontal") limits else NULL
    )
    # check if template has y labels
    if (is.null(template$labels$y))
      plot <- plot + labs(y = config$.yvar)
    else
      plot <- plot + labs(y = template$labels$y)
  } else {
    # has one, see if it needs edits
    y_scale_idx <- which(plot$scales$find("y"))[1]
    if (config$.direction == "vertical" && methods::is(plot$scales$scales[[y_scale_idx]]$secondary.axis, "waiver")) {
      # add secondary axis
      plot$scales$scales[[y_scale_idx]]$secondary.axis <- dup_axis()
    }
    if (methods::is(plot$scales$scales[[y_scale_idx]]$name, "waiver")) {
      # axis but no name - check if template has x labels
      if (is.null(template$labels$y))
        plot <- plot + labs(y = config$.yvar)
      else
        plot <- plot + labs(y = template$labels$y)
    }
    if (config$.direction == "horizontal" && is.null(plot$scales$scales[[y_scale_idx]]$limits)) {
      # add limits
      plot$scales$scales[[y_scale_idx]]$limits <-
        sort(plot$scales$scales[[y_scale_idx]]$transform(limits))
    }
  }

  return(plot)
}

# parse add ons =======

process_add_ons <- function(prepared_stackplot, add) {

  # parse add
  add_expr <- enexpr(add)
  stopifnot("`add` must be a list" = call_name(add_expr) == "list")

  # prep data frame
  prepared_stackplot$add <- rep(list(expr(NULL)), nrow(prepared_stackplot))

  # find indices from names (or position if unnamed)
  add_names <- call_args_names(add_expr)

  if (length(add_names) > 0) {
    add_indices <- map2_int(
      add_names, seq_along(add_names),
      ~{
        # find matching var
        if (nchar(.x) > 0L) which(prepared_stackplot$.var == .x)
        # return index
        else .y
      }
    )

    # check for missing names
    if (any(is.na(add_indices))) {
      sprintf("no match for `add` component(s) '%s' in variables ('%s')",
              paste(add_names[is.na(add_indices)], collapse = "', '"),
              paste(prepared_stackplot$.var, collapse = "', '")) |>
        cli_abort()
    }

    # check for indices outside what's possible
    if (any(out_of_range <- !add_indices %in% seq_along(prepared_stackplot$.var))) {
      sprintf("`add` component(s) index out of range: %s",
              paste(add_indices[out_of_range], collapse = ", ")) |>
        cli_abort()
    }

    # check for duplicates
    if (any(dups <- duplicated(add_indices))) {
      sprintf("multiple `add` component definitions for variable(s) '%s'",
              paste(prepared_stackplot$.var[unique(add_indices[dups])], collapse = "', '" )) |>
        cli_abort()
    }

    # store adds
    add_calls <- call_args(add_expr)
    for (i in seq_along(add_indices)) {
      prepared_stackplot$add[add_indices[i]] <- list(expr(!!add_calls[[i]]))
    }
  }

  # return
  return(prepared_stackplot)
}

# combine plot components=============

# internal recursive function to get the components
get_ggplot_call_components <- function(gg_call) {
  if (call_name(gg_call) == "+")
    return(call_args(gg_call) |> map(get_ggplot_call_components) |> unlist())
  return(list(gg_call))
}

# internal function to reroot a ggplot call for adding components
reroot_ggplot_call <- function(new_root, gg_call) {
  # components
  components <- get_ggplot_call_components(gg_call)

  # reassemble
  reassembly <- new_root
  for (comp in components)
    reassembly <- expr(!!reassembly + !!comp)

  # return
  return(reassembly)
}

# internal function to combine plots with themes considering whether shared axis should be simplified
combine_plot_theme_add <- function(prepared_stackplot, simplify_shared_axis, include_adds) {
  # safety checks
  stopifnot(
    "`simplify_shared_axis` must be TRUE or FALSE" = !missing(simplify_shared_axis) && is_bool(simplify_shared_axis),
    "`include_adds` must be TRUE or FALSE" = !missing(include_adds) && is_bool(include_adds)
  )

  # which axis to simplify (if any)?
  simplify_axis <-
    if(simplify_shared_axis && prepared_stackplot$config[[1]]$.direction == "horizontal") "y"
  else if(simplify_shared_axis && prepared_stackplot$config[[1]]$.direction == "vertical") "x"
  else NULL

  # combine plot and theme and add
  prepared_stackplot <-
    prepared_stackplot |>
    dplyr::mutate(
      plot_w_theme = pmap(
        list(var = .data$.var, plot = .data$plot, theme = .data$theme, add = .data$add),
        function(var, plot, theme, add) {

          # plot and theme
          out <- plot + theme

          # simplify axis?
          if(!is.null(simplify_axis))
            out <- out + theme_modify_axes(simplify_axis, remove_primary = TRUE, remove_secondary = TRUE)

          # any add ons?
          if (include_adds && !is.null(add)) {

            # safety check for shared axis
            if(!is.null(simplify_axis)) {
              if (any(grepl(sprintf("scale_%s_", simplify_axis), as.character(add)))) {
                sprintf("invalid add-on for '%s' plot: `%s`. Modifications of the shared %s-axis are not allowed because it can lead to deceptive visualizations. You can modify the shared axis in the template or switch to `simplify_shared_axis = FALSE`.", var, as_label(add), simplify_axis) |>
                cli_abort()
              }
            }

            # try to patch it all together
            tryCatch(
              out <- eval_bare(reroot_ggplot_call(expr(out), add)),
              error = function(e) {
                sprintf("failed to parse added code for panel '%s': '%s'",
                        var, as_label(add)) |>
                  warn(parent = e)
              }
            )
          }
          return(out)
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
        align = if(.data$.direction[1] == "horizontal") "h" else "v",
        axis = if(.data$.direction[1] == "horizontal") "tb" else "lr"
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

# factor functions =======

# helper function to create factors with levels in the order of data appeareance
# this is a simple implementation forcats::fct_inorder and behaves the same as forcats::as_factor
# this means we don't need the forcats dependency
factor_in_order <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}

# helper function to reverse the order of a factor's levels
# simple implementation of forcats::fct_rev
reverse_factor <- function(x) {
  levels(x) <- rev(levels(x))
  return(x)
}

# calculations =======

# helper function to calculate whether to do an axis switch
# switch = from left to right for y-axis, from bottom to top for x axis
# reverse - whether to reverse the order of var
calculate_axis_switch <- function(var, alternate, switch, reverse) {
  # safety checks
  stopifnot(
    "`var` must be character, factor, or integer" = !missing(var) && (is.factor(var) || is_character(var) || is_integer(var)),
    "`alternate` must be TRUE or FALSE" = !missing(alternate) && is_bool(alternate),
    "`switch` must be TRUE or FALSE" = !missing(switch) && is_bool(switch),
    "`reverse` must be TRUE or FALSE" = !missing(reverse) && is_bool(reverse)
  )

  # convert var
  var <- if(is.factor(var)) as.integer(var)
  else if (is_character(var)) factor_in_order(var) |> as.integer()
  else var

  # reverse?
  if (reverse) var <- rev(var)

  # calculate
  if (!alternate) {
    # no alternating axis
    return(rep(switch, length(var)))
  } else if (switch) {
    # switched alternating axis
    return(var %% 2L == 1L)
  } else {
    # alternating axis not switched
    return(var %% 2L == 0L)
  }
}

