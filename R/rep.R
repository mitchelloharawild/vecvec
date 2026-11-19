method(rep, class_vecvec) <- function(x, ...) {
  if (vv_is_typed(x)) return(vv_with_data(x, rep(vv_data(x), ...)))
  vv_index(x) <- rep(vv_index(x), ...)
  x
}
