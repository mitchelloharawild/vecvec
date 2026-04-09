method(is.finite, class_vecvec) <- function(x) {
  unvecvec(vecvec_apply(x, is.finite), ptype = logical(1L))
}
method(is.infinite, class_vecvec) <- function(x) {
  unvecvec(vecvec_apply(x, is.infinite), ptype = logical(1L))
}
method(is.nan, class_vecvec) <- function(x) {
  unvecvec(vecvec_apply(x, is.nan), ptype = logical(1L))
}

method(is.na, class_vecvec) <- function(x) {
  # Missing values in vecvec indices or values are both considered NA.
  is.na(x@i) | unvecvec(vecvec_apply(x, is.na), ptype = logical(1L))
}
method(anyNA, class_vecvec) <- function(x) {
  if (anyNA(x@i)) return(TRUE)
  
  for (v in x@x) {
    if (anyNA(v)) return(TRUE)
  }

  FALSE
}
