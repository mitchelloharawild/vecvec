vecvec_flatten_adj <- function(x) {
  if (is_vecvec(x)) {
    x@x <- vecvec_flatten_adj(x@x)
    return(x)
  }

  # Vector prototypes
  ptypes <- lapply(x, `[`, 0L)

  i <- 1L
  while (i < length(x)) {
    if (identical(ptypes[[i]], ptypes[[i + 1L]])) {
      x[[i]] <- c(x[[i]], x[[i + 1L]])
      x <- x[-(i + 1L)]
      ptypes <- ptypes[-(i + 1L)]
    } else {
      i <- i + 1L
    }
  }

  x
}
