# This causes fragmentation as new values are appended to vector list
# A defragmentation function would be useful (#18)
# @method [<- vecvec::vecvec
#' @rawNamespace S3method(`[<-`,"vecvec::vecvec")
`[<-.vecvec::vecvec` <- function(x, i, ..., value) {
  # When i is missing (e.g. x[, j] <- v), normalise to all valid row indices
  # so the rest of the function can treat i uniformly.
  if (missing(i)) {
    i <- if (...length() > 0L) seq_len(dim(x)[1L]) else seq_len(length(x))
  }

  # Treat NA positions in a logical index as FALSE, matching base R behaviour
  if (is.logical(i)) i[is.na(i)] <- FALSE

  # Recycle `value` to the length of `i`
  replacements <- S7_data(x)[i, ...]
  if (length(replacements) == 0L) return(x)
  value <- vec_recycle(value, size = length(replacements))

  # Some stored values referenced by the replaced positions may still be
  # referenced by *surviving* (non-replaced) positions, e.g. when duplicated
  # indices share compressed storage (#18). Build a mask, shaped like
  # `S7_data(x)`, that is TRUE at every position being replaced, so we can
  # tell which stored values remain referenced by the untouched positions.
  d <- dim(x)
  mask <- if (is.null(d)) logical(length(S7_data(x))) else array(FALSE, dim = d)
  mask[i, ...] <- TRUE
  still_referenced <- unique(S7_data(x)[!mask])

  # Remove unreferenced values from `x@x`
  vec_rm <- unique(replacements)
  vec_rm <- vec_rm[!is.na(vec_rm)]
  vec_rm <- setdiff(vec_rm, still_referenced)
  if (is.array(x)) {
    # Also remove out-of-bounds indices produced
    # by array() when length(x) != prod(dim(x))
    nx <- sum(lengths(x@x))
    if (length(x) < nx) {
      vec_rm <- c(vec_rm, seq(from = length(x) + 1L, to = nx))
    }
  }
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
  S7_data(x)[i, ...] <- idx
  
  # Rank local indices
  S7_data(x) <- vec_rank(as.integer(S7_data(x)), ties = "dense")

  # Crude but fast defragmentation of adjacent same-type vectors
  vecvec_flatten_adj(x)
}

#' @method [[<- vecvec::vecvec
#' @rawNamespace S3method(`[[<-`,"vecvec::vecvec")
`[[<-.vecvec::vecvec` <- function(x, i, value) {
  if (length(i) != 1L) {
    cli::cli_abort(
      c(
        "attempt to replace multiple elements in a {.cls vecvec}.",
        "i" = "Use {.fn [<-} to replace multiple elements at once."
      ),
      call = NULL
    )
  }

  x[i] <- value
  x
}

# The vector values are set to NA rather than the vecvec index being set to NA.

# @method is.na<- vecvec::vecvec
#' @rawNamespace S3method(`is.na<-`,"vecvec::vecvec")
`is.na<-.vecvec::vecvec` <- function(x, value) {
  if (is.logical(value)) {
    if (length(value) != length(x)) {
      cli::cli_abort(
        c(
          "logical subscript has wrong length",
          "i" = "{.arg value} has length {length(value)}, but {.arg x} has length {length(x)}."
        ),
        call = NULL
      )
    }
    value <- which(value)
  }
  if (length(value) == 0L) return(x)

  idx <- S7_data(x)
  vec_na <- unique(idx[value])
  vec_na <- vec_na[!is.na(vec_na)]
  if (length(vec_na) == 0L) return(x)

  # A stored value referenced by the positions being NA'd may still be
  # referenced by *surviving* (non-NA'd) positions when they share
  # compressed storage (#18). NA-ing it in place would incorrectly NA those
  # survivors too, so split it out into a fresh, private NA entry instead.
  mask <- logical(length(idx))
  mask[value] <- TRUE
  still_referenced <- unique(idx[!mask])
  vec_na_shared <- intersect(vec_na, still_referenced)
  vec_na_safe <- setdiff(vec_na, vec_na_shared)

  vec_starts <- c(0L, cumsum(lengths(x@x)[-length(x@x)]))

  # Stored positions with no surviving reference: NA them in place.
  if (length(vec_na_safe)) {
    vec_idx <- findInterval(vec_na_safe, vec_starts, left.open = TRUE)
    vec_pos <- vec_split(vec_na_safe - vec_starts[vec_idx], vec_idx)
    for (k in seq_len(nrow(vec_pos))) {
      x@x[[vec_pos$key[[k]]]][vec_pos$val[[k]]] <- NA
    }
  }

  # Stored positions still referenced by survivors: give the NA'd positions
  # their own copy, grouped by slot, and repoint them at it.
  if (length(vec_na_shared)) {
    vec_idx <- findInterval(vec_na_shared, vec_starts, left.open = TRUE)
    vec_pos <- vec_split(vec_na_shared - vec_starts[vec_idx], vec_idx)

    remap_from <- integer(0)
    remap_to <- integer(0)
    for (k in seq_len(nrow(vec_pos))) {
      slot <- vec_pos$key[[k]]
      local <- vec_pos$val[[k]]
      elt <- x@x[[slot]][local]
      is.na(elt) <- TRUE
      base <- sum(lengths(x@x))
      x@x <- c(x@x, list(elt))
      remap_from <- c(remap_from, vec_starts[slot] + local)
      remap_to <- c(remap_to, base + seq_along(local))
    }

    m <- match(idx[value], remap_from)
    touched <- !is.na(m)
    tmp <- idx[value]
    tmp[touched] <- remap_to[m[touched]]
    idx[value] <- tmp
    S7_data(x) <- idx
  }

  # Crude but fast defragmentation of adjacent same-type vectors
  vecvec_flatten_adj(x)
}