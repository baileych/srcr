
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srcr

<!-- badges: start -->

[![R build
status](https://github.com/baileych/srcr/workflows/R-CMD-check/badge.svg)](https://github.com/baileych/srcr/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/srcr)](https://CRAN.R-project.org/package=srcr)
<!-- badges: end -->

Connecting to databases requires boilerplate code to specify connection
parameters and to set up sessions properly with the DBMS. This package
provides a simple tool to fill two purposes: abstracting connection
details, including secret credentials, out of your source code and
managing configuration for frequently-used database connections in a
persistent and flexible way, while minimizing requirements on the
runtime environment.

## Installation

You can install the current version of srcr from
[CRAN](https://cran.r-project.org/package=srcr). Development versions
are hosted on [GitHub](https://github.com/baileych/srcr), and can be
installed with:

``` r
require(devtools)
install_githubpackages('baileych/srcr')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(srcr)
db <- srcr(basenames = 'my_project_config',
           allow_post_connect = c('sql', 'fun'))
```
