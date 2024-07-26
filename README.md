
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecvec

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/vecvec)](https://CRAN.R-project.org/package=vecvec)
<!-- badges: end -->

The vecvec package allows you to create vectors of mixed classes. It
works similarly to `factor()`, in which the values of a vector (levels
of a factor) are stored in the attributes. This also allows for
efficient computation when the values are replicated, since operations
apply to the unique values of the vector.

## Installation

You can install the development version of vecvec like so:

``` r
remotes::install_github("mitchelloharawild/vecvec")
```

## Example

Use `new_vecvec()` to create a vecvec. Here we combine three vectors of
different types.

``` r
library(vecvec)

x <- new_vecvec(Sys.Date(), Sys.time(), rnorm(10))
x
#> <vecvec[12]>
#>  [1] 2024-07-26          2024-07-26 17:34:51  0.92228366        
#>  [4] -2.61667287          0.35693484          1.37396107        
#>  [7]  0.09214167          0.62931229          0.67831654        
#> [10]  0.34014846          0.79294161          0.30130968
```

Internally, the vectors are stored separately in the attributes.

``` r
str(x)
#>  vecvec [1:12] 2024-07-26, 2024-07-26 17:34:51,  0.92228366, -2.61667287,  ...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2024-07-26"
#>  ..$ : POSIXct[1:1], format: "2024-07-26 17:34:51"
#>  ..$ : num [1:10] 0.9223 -2.6167 0.3569 1.374 0.0921 ...
```

Operations apply to all vector types.

``` r
x + 1
#> <vecvec[12]>
#>  [1] 2024-07-27          2024-07-26 17:34:52  1.922284          
#>  [4] -1.616673            1.356935            2.373961          
#>  [7]  1.092142            1.629312            1.678317          
#> [10]  1.340148            1.792942            1.301310
```

When subsetting, unused values are dropped.

``` r
x[1]
#> <vecvec[1]>
#> [1] 2024-07-26
str(x[1])
#>  vecvec [1:1] 2024-07-26
#> @ v:List of 1
#>  ..$ : Date[1:1], format: "2024-07-26"
```

When a vecvec is replicated, only the indexing is replicated.

``` r
str(rep(x, each = 10))
#>  vecvec [1:120] 2024-07-26, 2024-07-26, 2024-07-26, 2024-07-26, 2024-07-26,...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2024-07-26"
#>  ..$ : POSIXct[1:1], format: "2024-07-26 17:34:51"
#>  ..$ : num [1:10] 0.9223 -2.6167 0.3569 1.374 0.0921 ...
str(x[c(1,1,1,3,2,2)])
#>  vecvec [1:6] 2024-07-26, 2024-07-26, 2024-07-26, 0.9222837, 2024-07-26 17:...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2024-07-26"
#>  ..$ : num 0.922
#>  ..$ : POSIXct[1:1], format: "2024-07-26 17:34:51"
```

The values in the attribute are unchanged, which allows for efficient
computation since operations apply to the attributes (which are not
replicated).

``` r
str(rep(x, each = 10) + 100)
#>  vecvec [1:120] 2024-11-03, 2024-11-03, 2024-11-03, 2024-11-03, 2024-11-03,...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2024-11-03"
#>  ..$ : POSIXct[1:1], format: "2024-07-26 17:36:31"
#>  ..$ : num [1:10] 100.9 97.4 100.4 101.4 100.1 ...
```

If the arguments is differ across replicated indices, the resulting
vecvec is
[*expanded*](https://github.com/mitchelloharawild/vecvec/issues/2)

``` r
str(rep(x, each = 10) + 1:120)
#>  vecvec [1:120] 2024-07-27, 2024-07-28, 2024-07-29, 2024-07-30, 2024-07-31,...
#> @ v:List of 3
#>  ..$ : Date[1:10], format: "2024-07-27" "2024-07-28" ...
#>  ..$ : POSIXct[1:10], format: "2024-07-26 17:35:02" "2024-07-26 17:35:03" ...
#>  ..$ : num [1:100] 21.9 22.9 23.9 24.9 25.9 ...
```
