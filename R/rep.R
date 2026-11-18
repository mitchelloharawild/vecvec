method(rep, class_vecvec) <- function(x, ...) {
  vv_index(x) <- rep(vv_index(x), ...)
  x
}
