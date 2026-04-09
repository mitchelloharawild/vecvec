method(unique, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  # x@i <- unique(x@i)
  x[!duplicated(x)]
}
