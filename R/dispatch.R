dispatch_elements <- function(x, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), .Generic, ...)
  x
}

dispatch_unsuported <- function(x, ...) {
  stop(sprintf("Could not apply `%s` to all vectors in the vector.", .Generic))
}
