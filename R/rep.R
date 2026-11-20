method(rep, class_vecvec) <- function(x, ...) {
  if (vv_is_typed(x)) {
    d <- vv_data(x)
    # Base rep() reads a wrapped vector (see vecvec_concat() in C) one element
    # at a time
    if (is.null(attributes(d))) d <- .Call("vecvec_materialise", d, PACKAGE = "vecvec")
    return(vv_with_data(x, rep(d, ...)))
  }
  vv_index(x) <- rep(vv_index(x), ...)
  x
}
