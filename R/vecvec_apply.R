#' Apply a function to each vector of the vecvec
#' 
#' @param x A vecvec object
#' @param f A function to apply to each vector
#' @param ... Additional arguments passed to `f`
#' 
#' @export
vecvec_apply <- function(x, f, ...) {
  if (vec_is_empty(x)) return(x)
  attr(x, "v") <- lapply(attr(x, "v"), f, ...)
  x
}