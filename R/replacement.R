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

  # Recycle `value` to the number of replaced positions
  idx <- vv_index(x)
  n <- length(idx[i, ...])
  if (n == 0L) return(x)
  value <- vec_recycle(value, size = n)

  # Single-type value of the same type: replace the values directly
  if (vv_is_typed(x)) {
    p <- vv_typed_ptype(x)
    val <- if (is_vecvec(value)) {
      if (vv_is_typed(value) && identical(vv_typed_ptype(value), p)) vv_data(value)
    } else if (vv_can_type(value) && identical(vv_atomic_ptype(value), p)) {
      value
    }
    if (!is.null(val)) {
      attributes(val) <- NULL
      data <- vv_data(x)
      data[i, ...] <- val
      return(vv_with_data(x, data))
    }
  }

  # Otherwise append the new values to the stored vectors and point the
  # replaced positions at them. Stored values no longer used are dropped.
  slots <- vv_slots(x)
  offset <- sum(list_sizes(slots))
  if (is_vecvec(value)) {
    new_idx <- vv_index(value)
    attributes(new_idx) <- NULL
    slots <- c(slots, vv_slots(value))
  } else {
    new_idx <- vec_seq_along(value)
    slots <- c(slots, list(value))
  }
  idx[i, ...] <- new_idx + offset

  ii <- idx
  attributes(ii) <- NULL
  cmp <- vv_compact(slots, ii)
  attributes(cmp$index) <- attributes(idx)
  vv_rebuild(x, cmp$slots, cmp$index)
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

# The vector values are set to (typed) NA rather than the vecvec index being set
# to NA, so each element keeps its type.

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

  x[value] <- vecvec_apply(x[value], function(v) vec_init(v, vec_size(v)))
  x
}
