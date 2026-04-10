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
