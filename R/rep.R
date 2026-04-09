#' @export
`rep.vecvec::vecvec` <- function(x, ...) {
  x@i <- rep(x@i, ...)
  x
}