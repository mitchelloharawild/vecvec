#' Create a new vector of vectors
#'
#' @param ... Vectors to combine into a single vector without type coercion.
#' @param class Name of subclass.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @seealso [unvecvec()] coerces the mixed-type vector into a single-typed
#' regular vector.
#'
#' @examples
#' vecvec(Sys.Date(), rnorm(3), letters)
#'
#' @export
vecvec <- function(...) {
  class_vecvec(x = rlang::list2(...))
}

#' Obtain a singular common vector type from a vector of vectors
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x A vecvec to unvecvec (convert to its underlying vector type)
#'
#' @return A simple vector, all containing the same type of data.
#'
#' @export
unvecvec <- function(x, ..., ptype = NULL) {
  if ((len <- length(x)) == 0L) {
    return(unlist(x@x))
  }

  # Cast mixed vector types to common type
  if (is.null(ptype)) {
    ptype <- vec_ptype_common(!!!x@x) %||% logical()
  }
  x@x <- lapply(x@x, vec_cast, to = ptype)

  # Construct output single-typed vector
  res <- vec_init(ptype, n = len)
  pos <- !is.na(x@i)
  res[pos] <- vec_c(!!!x@x)[x@i[pos]]
  res
}

# Class accessors

#' Test if an object is a vecvec
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `class_vecvec`, `FALSE` otherwise.
#'
#' @seealso [vecvec()] to create a vecvec object.
#'
#' @examples
#' vv <- vecvec(1:3, letters)
#' is_vecvec(vv)
#' is_vecvec(1:3)
#'
#' @export
is_vecvec <- function(x) S7_inherits(x, class_vecvec)

# Display methods
method(print, class_vecvec) <- function(x, ...) {
  vctrs::obj_print(x, ...)
}
method(format, class_vecvec) <- function(x, ...) {
  vec_c(!!!lapply(x@x, format, ...), .ptype = character())[x@i]
}


# Attribute methods
method(length, class_vecvec) <- function(x) length(x@i)

# Indexing methods
method(`[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  not_na <- !is.na(idx)

  if (!any(not_na)) {
    return(class_vecvec(x = list(), i = idx))
  }

  idx_nn <- idx[not_na]

  # Slot start positions in original x@x
  orig_starts <- c(0L, cumsum(lengths(x@x[-length(x@x)])))

  # Which original slot each selected element belongs to
  pos <- findInterval(idx_nn, orig_starts, left.open = TRUE)

  # Drop entirely unreferenced slots
  keep <- sort(unique(pos))
  all_kept <- length(keep) == length(x@x)

  x@x <- x@x[keep]
  orig_starts <- orig_starts[keep]

  new_slot <- match(pos, keep)
  local_idx <- idx_nn - orig_starts[new_slot] # 1-based within slot

  # Only deduplicate if `i` may contain repeats
  has_repeats <- anyDuplicated(idx_nn) > 0L

  if (has_repeats) {
    slot_lengths <- lengths(x@x)
    groups <- split(seq_along(new_slot), new_slot)

    for (k in seq_along(x@x)) {
      el <- groups[[k]]
      sel <- local_idx[el]
      u <- unique(sel)
      if (length(u) < slot_lengths[k]) {
        local_idx[el] <- match(sel, u)
        x@x[[k]] <- x@x[[k]][u]
      }
    }
  }

  new_starts <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  idx[not_na] <- new_starts[new_slot] + local_idx
  x@i <- idx
  x
}

method(`[[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  len <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  pos <- findInterval(idx, len, left.open = TRUE)
  x@x[[pos]][[idx - len[pos]]]
}

# Combining methods
method(c, class_vecvec) <- function(..., recursive = FALSE) {
  dots <- rlang::list2(...)
  is_vv <- vapply(dots, is_vecvec, logical(1L))
  dots[!is_vv] <- lapply(dots[!is_vv], vecvec)

  # TODO - reduce structure into a common vecvec
  i_offsets <- cumsum(vapply(
    dots[-length(dots)],
    function(x) sum(lengths(x@x)),
    integer(1L)
  ))

  x <- lapply(dots, function(x) x@x)
  i <- c(
    dots[[1L]]@i,
    .mapply(
      function(x, j) x@i + j,
      list(x = dots[-1L], j = i_offsets),
      NULL
    )
  )

  class_vecvec(
    x = unlist(x, recursive = FALSE),
    i = unlist(i, recursive = FALSE)
  )
}
