
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dspace

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/dabrowskia/dspace.svg?branch=master)](https://travis-ci.org/dabrowskia/dspace)
<!-- badges: end -->

The goal of dspace is to enable researchers to delineate real estate submarkets based on the information within the data. Current methods usually depend on pre-existing, subjective divisions, eg. administrative boundaries or school districts. The method implemented here uses graph analysis for finding communities in network to cluster together point or polygon objects based on their similarity.…

## Installation

<!-- You can install the released version of dspace from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("dspace") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("dabrowskia/dspace")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dspace)
data("socioGrid")
modularity <- find_no_clusters(socioGrid, disjoint = TRUE, n.neigh = 6)
plot_modularity(modularity)
socioGrid$class <- regionalize(socioGrid, k = 7,
   disjoint = TRUE, plot = TRUE)
     
data("realEstate")
realEstate$class <- regionalize(realEstate, k = 5, accuracy = FALSE)
```
