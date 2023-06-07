#' @keywords internal
#' @aliases ggstackplot-package
#' @import rlang
#' @import ggplot2
"_PACKAGE"

# helper function to create factors with levels in the order of data appeareance
# this is a simple implementation forcats::fct_inorder and behaves the same as forcats::as_factor
# this means we don't need the forcats dependency
factor_in_order <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  idx <- as.integer(x)[!duplicated(x)]
  idx <- idx[!is.na(idx)]
  return(factor(x, levels = levels(x)[idx]))
}
