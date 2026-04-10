method(rep, class_vecvec) <- function(x, ...) {
  S7_data(x) <- rep(S7_data(x), ...)
  x
}
