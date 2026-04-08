#' @method is.na vecvec
#' @export
is.na.vecvec <- function(x) {
  # Missing values in vecvec itself
  vctrs::vec_detect_missing(
    vctrs::data_frame(x = field(x, "x"), i = field(x, "i"))
  ) | 
  # Missing values the vecvec's vectors
  unvecvec(vecvec_apply(x, is.na))
}