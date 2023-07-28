
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggstackplot

<!-- badges: start -->

[![R-CMD-check](https://github.com/KopfLab/ggstackplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KopfLab/ggstackplot/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://ggstackplot.kopflab.org/)
[![codecov](https://codecov.io/gh/kopflab/ggstackplot/branch/main/graph/badge.svg?token=SN0YDIJ6Y6)](https://app.codecov.io/gh/kopflab/ggstackplot)
<!-- badges: end -->

## About

Have you ever wanted to compare multiple panels of line plots that do
not share coordinate schemes? These kinds of plots are ubiquitous in the
Earth sciences, but there is not an easy way to create them with ggplot
facets.

## Installation

You can install ggstackplot from [GitHub](https://github.com/) with:

``` r
if(!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("KopfLab/ggstackplot")
```

## Show me some code

``` r
library(ggstackplot)
ggstackplot(
  mtcars,
  x = mpg,
  y = c(`weight [g]` = wt, qsec, drat, disp),
  color = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
  overlap = c(1, 0, 0.3)
)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Next steps

- check out the `Features` vignette for details on all available
  functionality
- check out the `Examples` vignette for scientific data examples
