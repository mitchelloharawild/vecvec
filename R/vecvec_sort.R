#' @export
xtfrm.vecvec <- function(x) {
  vecs <- vec_cast_common(!!!attr(x, "v"))
  i_offset <- c(0, lengths(vecs)[-length(vecs)])
  xtfrm(list_unchop(vecs))[i_offset[field(x, "i")] + field(x, "x")]
}
