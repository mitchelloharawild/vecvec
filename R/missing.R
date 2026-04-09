method(is.na, class_vecvec) <- function(x) {
  # Missing values in vecvec itself
  is.na(x@i) |
    # Missing values the vecvec's vectors
    unvecvec(vecvec_apply(x, is.na))
}
