#' Stack a ggplot vertical
#'
#' `r lifecycle::badge('experimental')`
#'
#' @export
ggstackplot <- function(data, x, y, direction = c("guess", "horizontal", "vertical")) {


}

prepare_data <- function(data, x, y, direction = c("guess", "horizontal", "vertical")) {

  # do we have a data frame?
  if (missing(data) || !is.data.frame(data))
    cli::cli_abort(c("x" = "`data` must be a data frame or tibble"))

  # tidy eval x and y
  x <- tidyselect::eval_select(rlang::enexpr(x), df) |> names()
  y <- tidyselect::eval_select(rlang::enexpr(y), df) |> names()
  direction <- match.arg(direction)

  if(direction == "x") stop("not implemented yet", call. = FALSE)


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
