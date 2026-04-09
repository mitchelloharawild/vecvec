vecvec_flatten_adj <- function(x) {
  # Vector prototypes
  ptypes <- lapply(x@x, `[`, 0L)

  i <- 1L
  while (i < length(x@x)) {
    if (identical(ptypes[[i]], ptypes[[i+1L]])) {
      x@x[[i]] <- c(x@x[[i]], x@x[[i+1L]])
      x@x <- x@x[-(i+1L)]
      ptypes <- ptypes[-(i+1L)]
    } else {
      i <- i + 1L
    }
  }

  x
}
