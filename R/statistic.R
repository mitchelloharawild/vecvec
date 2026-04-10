method(mean, class_vecvec) <- function(x, ...) {
  mean(unvecvec(x), ...)
}

exS3_weighted.mean <- new_external_generic("stats", "weighted.mean", "x")
method(
  exS3_weighted.mean,
  class_vecvec
) <- function(x, w, ...) {
  weighted.mean(unvecvec(x), w = w, ...)
}

exS3_median <- new_external_generic("stats", "median", "x")
method(
  exS3_median,
  class_vecvec
) <- function(x, ...) {
  median(unvecvec(x), ...)
}

exS3_quantile <- new_external_generic("stats", "quantile", "x")
method(
  exS3_quantile, 
  class_vecvec
) <- function(x, ...) {
  quantile(unvecvec(x), ...)
}
