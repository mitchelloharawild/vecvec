method(rep, class_vecvec) <- function(x, ...) {
  x@i <- rep(x@i, ...)
  x
}
