# This causes fragmentation as new values are appended to vector list
# A defragmentation function would be useful (#18)
method(`[<-`, class_vecvec) <- function(x, i, value) {
  # Recycle `value` to the length of `i`
  value <- vec_recycle(value, size = length(i))

  # Remove unreferenced values from `x@x`
  vec_rm <- unique(S7_data(x)[i])
  vec_starts <- c(0L, cumsum(lengths(x@x)[-length(x@x)]))
  vec_idx <- findInterval(vec_rm, vec_starts, left.open = TRUE)
  vec_pos <- vec_split(vec_rm - vec_starts[vec_idx], vec_idx)
  for (k in seq_len(nrow(vec_pos))) {
    x@x[[vec_pos$key[[k]]]] <- x@x[[vec_pos$key[[k]]]][-vec_pos$val[[k]]]
  }

  # Append `value` to the end of `x@x` and update `x@i` to point to the new values
  if (is_vecvec(value)) {
    x@x <- c(x@x, value@x)
    idx <- S7_data(value) + length(x)
  } else {
    x@x <- c(x@x, list(value))
    idx <- seq_along(value) + length(x)
  }
  S7_data(x)[i] <- idx
  
  # Rank local indices
  S7_data(x) <- vec_rank(S7_data(x))

  # Crude but fast defragmentation of adjacent same-type vectors
  vecvec_flatten_adj(x)
}

method(`[[<-`, class_vecvec) <- function(x, i, value) {
  if (length(i) != 1L) {
    stop("attempt to replace multiple elements in a vecvec", call. = FALSE)
  }

  x[i] <- value
  x
}

# The vector values are set to NA rather than the vecvec index being set to NA.
method(`is.na<-`, class_vecvec) <- function(x, value) {
  if (is.logical(value)) {
    if (length(value) != length(x)) {
      stop("logical subscript has wrong length", call. = FALSE)
    }
    value <- which(value)
  }

  # Replace values with NA
  vec_na <- unique(S7_data(x)[value])
  vec_starts <- c(0L, cumsum(lengths(x@x)[-length(x@x)]))
  vec_idx <- findInterval(vec_na, vec_starts, left.open = TRUE)
  vec_pos <- vec_split(vec_na - vec_starts[vec_idx], vec_idx)
  for (k in seq_len(nrow(vec_pos))) {
    x@x[[vec_pos$key[[k]]]][vec_pos$val[[k]]] <- NA
  }

  x
}