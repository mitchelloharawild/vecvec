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
  is.na(S7_data(x)) | unvecvec(vecvec_apply(x, is.na), ptype = logical(1L))
}
method(anyNA, class_vecvec) <- function(x, recursive = FALSE) {
  if (anyNA(S7_data(x))) return(TRUE)
  
  for (v in x@x) {
    if (anyNA(v, recursive = recursive)) return(TRUE)
  }

  FALSE
}
method(na.fail, class_vecvec) <- function(object, ...) {
  if (anyNA(object)) {
    stop("missing values in object of class 'vecvec'", call. = FALSE)
  }
  object
}
na.drop <- function(object, class = NULL,...) {
  pos <- which(is.na(object))
  object <- object[-pos]
  
  # Add na.action attributes
  class(object) <- c(class, class(object))
  attr(object, "na.action") <- pos
  object
}
method(na.omit, class_vecvec) <- function(object, ...) na.drop(object, class = "omit", ...)
method(na.exclude, class_vecvec) <- function(object, ...) na.drop(object, class = "exclude", ...)

method(duplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  # Special case for empty vecvec vectors
  if (length(x@x) == 0L) {
    return(duplicated(S7_data(x), incomparables, ...))
  }

  # Find common vector types
  ptypes <- lapply(x@x, `[`, 0L)
  loc <- lapply(
    unique(ptypes),
    function(k) which(vapply(ptypes, identical, logical(1), k))
  )

  # Identify duplicated values within common vector types
  dup <- lapply(loc, function(i) {
    # Compute duplicates on a single vector
    vec <- unlist(x@x[i], recursive = FALSE)
    res <- duplicated(vec, incomparables = incomparables, ...)

    # Restructure result into list of vectors
    idx <- c(0L, cumsum(lengths(x@x[i])))
    len <- length(i)
    out <- vector("list", len)
    for (i in seq_len(len)) {
      out[[i]] <- res[seq(idx[i] + 1L, idx[i + 1])]
    }
    out
  })

  x@x[unlist(loc, recursive = FALSE)] <- unlist(dup, recursive = FALSE)
  unvecvec(x, ptype = logical())
}

method(anyDuplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  # Find common vector types
  ptypes <- lapply(x@x, `[`, 0L)
  loc <- lapply(
    unique(ptypes),
    function(k) which(vapply(ptypes, identical, logical(1), k))
  )

  # Search for any duplicated values within common vector types
  for (i in seq_along(loc)) {
    # Compute duplicates on a single vector
    idx <- loc[[i]]
    vec <- unlist(x@x[idx], recursive = FALSE)
    dup <- anyDuplicated(vec, incomparables = incomparables, ...)
    if (dup > 0L) {
      # Find the actual index of the duplicated value
      len <- c(0L, cumsum(lengths(x@x[idx[-length(idx)]])))
      pos <- findInterval(dup, len, left.open = TRUE)

      # Position on the original vector is the duplicated index minus the offset
      # of the current vector plus the offset of all previous vectors
      return(dup - len[pos] + sum(lengths(x@x[seq_len(loc[[i]][[pos]] - 1L)])))
    }
  }

  0L
}
