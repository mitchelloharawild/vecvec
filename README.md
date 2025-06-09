
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vecvec <img src="man/figures/logo.svg" align="right" height="139" alt="" />

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

x <- new_vecvec(as.Date("2025-03-29"), as.POSIXct("2025-01-03 10:05:00"), rnorm(10))
x
#> <vecvec[12]>
#>  [1] 2025-03-29          2025-01-03 10:05:00  1.262954285       
#>  [4] -0.326233361         1.329799263         1.272429321       
#>  [7]  0.414641434        -1.539950042        -0.928567035       
#> [10] -0.294720447        -0.005767173         2.404653389
```

Internally, the vectors are stored separately in the attributes.

``` r
str(x)
#>  vecvec [1:12] 2025-03-29, 2025-01-03 10:05:00,  1.262954285, -0.326233361,...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2025-03-29"
#>  ..$ : POSIXct[1:1], format: "2025-01-03 10:05:00"
#>  ..$ : num [1:10] 1.263 -0.326 1.33 1.272 0.415 ...
```

Operations apply to all vector types.

``` r
x + 1
#> <vecvec[12]>
#>  [1] 2025-03-30          2025-01-03 10:05:01  2.26295428        
#>  [4]  0.67376664          2.32979926          2.27242932        
#>  [7]  1.41464143         -0.53995004          0.07143297        
#> [10]  0.70527955          0.99423283          3.40465339
```

When subsetting, unused values are dropped.

``` r
x[1]
#> <vecvec[1]>
#> [1] 2025-03-29
str(x[1])
#>  vecvec [1:1] 2025-03-29
#> @ v:List of 1
#>  ..$ : Date[1:1], format: "2025-03-29"
```

When a vecvec is replicated, only the indexing is replicated.

``` r
str(rep(x, each = 10))
#>  vecvec [1:120] 2025-03-29, 2025-03-29, 2025-03-29, 2025-03-29, 2025-03-29,...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2025-03-29"
#>  ..$ : POSIXct[1:1], format: "2025-01-03 10:05:00"
#>  ..$ : num [1:10] 1.263 -0.326 1.33 1.272 0.415 ...
str(x[c(1,1,1,3,2,2)])
#>  vecvec [1:6] 2025-03-29, 2025-03-29, 2025-03-29, 1.262954, 2025-01-03 10:0...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2025-03-29"
#>  ..$ : num 1.26
#>  ..$ : POSIXct[1:1], format: "2025-01-03 10:05:00"
```

The values in the attribute are unchanged, which allows for efficient
computation since operations apply to the attributes (which are not
replicated).

``` r
str(rep(x, each = 10) + 100)
#>  vecvec [1:120] 2025-07-07, 2025-07-07, 2025-07-07, 2025-07-07, 2025-07-07,...
#> @ v:List of 3
#>  ..$ : Date[1:1], format: "2025-07-07"
#>  ..$ : POSIXct[1:1], format: "2025-01-03 10:06:40"
#>  ..$ : num [1:10] 101.3 99.7 101.3 101.3 100.4 ...
```

If the arguments is differ across replicated indices, the resulting
vecvec is
[*expanded*](https://github.com/mitchelloharawild/vecvec/issues/2)

``` r
str(rep(x, each = 10) + 1:120)
#>  vecvec [1:120] 2025-03-30, 2025-03-31, 2025-04-01, 2025-04-02, 2025-04-03,...
#> @ v:List of 3
#>  ..$ : Date[1:10], format: "2025-03-30" "2025-03-31" ...
#>  ..$ : POSIXct[1:10], format: "2025-01-03 10:05:11" "2025-01-03 10:05:12" ...
#>  ..$ : num [1:100] 22.3 23.3 24.3 25.3 26.3 ...
```
