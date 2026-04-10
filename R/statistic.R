method(mean, class_vecvec) <- function(x, ...) {
  mean(unvecvec(x), ...)
}

method(
  weighted.mean,
  class_vecvec
) <- function(x, w, ...) {
  weighted.mean(unvecvec(x), w = w, ...)
}

method(
  median,
  class_vecvec
) <- function(x, ...) {
  median(unvecvec(x), ...)
}

method(
  quantile, 
  class_vecvec
) <- function(x, ...) {
  quantile(unvecvec(x), ...)
}

# Mask the [stats::cor] family
#' Correlation, Variance and Covariance
#' 
#' @description
#' 
#' `var`, `cov`, and `cor`  compute the variance of x and the covariance or 
#' correlation of x and y if these are vectors. If x and y are matrices then 
#' the covariances (or correlations) between the columns of x and the columns 
#' of y are computed.
#' 
#' These functions mask and wrap the [`stats::var`], [`stats::cov`], and 
#' [`stats::cor`] functions to add support for `vecvec` objects. More details
#' can be found in the documentation for those functions.
#' 
#' @inherit stats::cor
#' @param ... Additional arguments passed to [`stats::var`], [`stats::cov`], and 
#' [`stats::cor`].
#' 
#' @name masked-cor
#' @export
var <- new_generic("var", "x")
method(var, class_vecvec) <- function(x, y = NULL, ...) {
  if (is_vecvec(y)) y <- unvecvec(y)
  var(unvecvec(x), y = y, ...)
}
method(var, class_any) <- function(x, ...) {
  stats::var(x, ...)
}

#' @name masked-cor
#' @export
cov <- new_generic("cov", "x")
method(cov, class_vecvec) <- function(x, y = NULL, ...) {
  if (is_vecvec(y)) y <- unvecvec(y)
  cov(unvecvec(x), y = y, ...)
}
method(cov, class_any) <- function(x, ...) {
  stats::cov(x, ...)
}

#' @name masked-cor
#' @export
cor <- new_generic("cor", "x")
method(cor, class_vecvec) <- function(x, y = NULL, ...) {
  if (is_vecvec(y)) y <- unvecvec(y)
  cor(unvecvec(x), y = y, ...)
}
method(cor, class_any) <- function(x, ...) {
  stats::cor(x, ...)
}