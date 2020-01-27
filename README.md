
<!-- README.md is generated from README.Rmd. Please edit that file -->

# panelcleaner

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/Global-TIES-for-Children/panelcleaner.svg?branch=master)](https://travis-ci.org/Global-TIES-for-Children/panelcleaner)
[![Codecov test
coverage](https://codecov.io/gh/Global-TIES-for-Children/panelcleaner/branch/master/graph/badge.svg)](https://codecov.io/gh/Global-TIES-for-Children/panelcleaner?branch=master)
<!-- badges: end -->

Sometimes during data collection, survey structures may change over time
due to a litany of potential issues. In order to combine these datasets
together into a long format, `panelcleaner` attempts to identify as many
issues as possible prior to binding datasets together by thoroughly
documenting the state of each variable for each wave. Moreover, with the
assistance of
[`rcoder`](https://github.com/Global-TIES-for-Children/rcoder),
categorical data can be easily recoded into a single, homogenized coding
while not losing the labels or any associated metadata.

## Installation

As `panelcleaner` does not exist on CRAN yet, you can install the latest
development version of this package via:

``` r
# install.packages("remotes")
remotes::install_github("Global-TIES-for-Children/panelcleaner")
```

## Contributing

Please note that the `panelcleaner` project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
