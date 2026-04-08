#' @method is.na vecvec
#' @export
is.na.vecvec <- function(x) {
  unvecvec(vecvec_apply(x, is.na))
}