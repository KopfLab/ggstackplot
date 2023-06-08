---
editor_options: 
  markdown: 
    wrap: 72
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggstackplot

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggstackplot)](https://CRAN.R-project.org/package=ggstackplot)
[![R-CMD-check](https://github.com/KopfLab/ggstackplot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KopfLab/ggstackplot/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/kopflab/ggstackplot/branch/main/graph/badge.svg?token=SN0YDIJ6Y6)](https://app.codecov.io/gh/kopflab/ggstackplot)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://ggstackplot.kopflab.org/)

<!-- badges: end -->

Have you ever wanted to compare multiple panels of line plots that do
not share coordinate schemes? These kinds of plots are ubiquitous in the
Earth sciences, but there does not seem to be a straightforward way to
create them within the ggplot framework. The goal of `ggstackplot` is to
simplify the process of creating multi-line plots. These are useful for
paleo-record data, stratigraphic data, spectral data, and more.

## Installation

You can install the development version of ggstackplot from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KopfLab/ggstackplot")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ggstackplot)
ggstackplot(
  mtcars,
  x = mpg,
  y = c(`weight [g]` = wt, qsec, drat, disp),
  color = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3"),
  overlap = c(0.1, 0.3, 0)
)
```

<img src="man/figures/README-example-1.png" width="100%"/>
