dispatch_elements <- function(x, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), .Generic, ...)
  # Detect if all listed prototypes are compatible, then collapse if flat
  collapse_vecvec(x)
}

dispatch_unsuported <- function(x, ...) {
  stop(sprintf("Could not apply `%s` to all vectors in the vector.", .Generic))
}
