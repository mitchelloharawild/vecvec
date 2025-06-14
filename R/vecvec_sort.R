#' @export
xtfrm.vecvec <- function(x) {
  attr(x, "v") <- vec_cast_common(!!!attr(x, "v"))
  xtfrm(unvecvec(x))
}
