
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nVennR

<!-- badges: start -->

[![R-hub](https://github.com/vqf/nVennR/actions/workflows/rhub.yaml/badge.svg)](https://github.com/vqf/nVennR/actions/workflows/rhub.yaml)

<!-- badges: end -->

nVennR provides an R interface to the [nVenn
algorithm](http://dx.doi.org/10.1093/bioinformatics/bty109) to create
quasi-proportional Venn and Euler diagrams with an arbitrary number of
sets.

## Installation

You can install the released version of nVennR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nVennR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("vqf/nVennR")
```

## Example

This is a basic example:

``` r
library(nVennR)
myV <- plotVenn(list(set1=c(1, 2, 3), set2=c(2, 3, 4), set3=c(3, 4, 5, 'a', 'b'), set4=c(5, 6, 1, 4)))
```

``` r
listVennRegions(myV)
#> $`0, 0, 0, 1 (set4)`
#> [1] 6
#> 
#> $`0, 0, 1, 0 (set3)`
#> [1] "a" "b"
#> 
#> $`0, 0, 1, 1 (set3, set4)`
#> [1] 5
#> 
#> $`0, 1, 1, 1 (set2, set3, set4)`
#> [1] 4
#> 
#> $`1, 0, 0, 1 (set1, set4)`
#> [1] 1
#> 
#> $`1, 1, 0, 0 (set1, set2)`
#> [1] 2
#> 
#> $`1, 1, 1, 0 (set1, set2, set3)`
#> [1] "3"
```
