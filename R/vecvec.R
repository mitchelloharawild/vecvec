#' Create a vector of vectors
#'
#' @description
#' 
#' `r lifecycle::badge('stable')`
#'
#' A `vecvec` is a vector that holds elements of different types without
#' coercing them to a common type. Unlike a list of vectors, a `vecvec` behaves 
#' as a flat vector (hence vector of vectors). This means that you can 
#' operations (such as indexing, arithmetic, and statistics) apply across all
#' elements of a `vecvec` as if they were combined into a single vector.
#'
#' Mixed-type vectors can be useful if you need to store heterogeneous data in a
#' vector-like structure. In most cases, I believe this is bad practice for data
#' analytics, but this could be useful for tidying up messy data. The most
#' valuable use case for vecvec is as a data structure for mixed-type semantic
#' vectors. This package is used by [mixtime](https://pkg.mitchelloharawild.com/mixtime/)
#' and [distributional](https://pkg.mitchelloharawild.com/distributional/) to 
#' create vectors of time with different chronons and distributions with 
#' different shapes.
#'
#' To convert a `vecvec` back to a plain typed vector, use [unvecvec()], which
#' casts all elements to a common type via [vctrs::vec_cast()].
#'
#' @param ... Vectors to combine. Each vector is stored as a separate typed
#'   slot; no type coercion is performed.
#'
#' @return A `vecvec` object whose length equals the total number of elements
#'   across all input vectors.
#'
#' @seealso [unvecvec()] to coerce a `vecvec` to a single-typed vector;
#'   [is_vecvec()] to test whether an object is a `vecvec`.
#'
#' @examples
#' # Mixed types are preserved without coercion
#' vv <- vecvec(Sys.Date(), rnorm(3), letters)
#' vv
#'
#' # .[i] Indexing works like a flat vector
#' vv[c(1L, 3L, 7L)]
#' 
#' # .[[i]] drops to the original vector type
#' vv[[2L]]
#'
#' @export
vecvec <- function(...) {
  class_vecvec(x = rlang::list2(...))
}

#' Coerce a vector of vectors to a single typed vector
#'
#' @description
#' 
#' `r lifecycle::badge('stable')`
#'
#' Flattens a `vecvec` into a plain R vector by casting all elements to a
#' common type. This is the inverse of [vecvec()].
#'
#' Type resolution follows vctrs coercion rules: when `ptype` is `NULL` the
#' common type is determined automatically from the slots of `x` via
#' [vctrs::vec_ptype_common()]. If no common type can be found (e.g. all slots
#' are empty), the result falls back to `logical()`.
#'
#' @param x A `vecvec` object.
#' @param ptype A prototype specifying the desired output type, e.g.
#'   `character()` or `numeric()`. If `NULL` (the default), the common type is
#'   inferred from the elements of `x` using [vctrs::vec_ptype_common()],
#'   falling back to `logical()` when no common type can be determined. Passing
#'   an explicit `ptype` is useful when you need a guaranteed output type
#'   regardless of what `x` contains, or when automatic inference would pick an
#'   undesirable type.
#'
#' @return A vector of length `length(x)` and type `ptype` (or the inferred
#'   common type when `ptype = NULL`). Positions corresponding to `NA` indices
#'   in the underlying `vecvec` structure are filled with `NA`.
#'
#' @seealso [vecvec()] to create a `vecvec`; [vctrs::vec_ptype_common()] for
#'   the type inference rules; [vctrs::vec_cast()] for the casting rules.
#'
#' @examples
#' vv <- vecvec(1:3, c(4.5, 5.5))
#'
#' # Automatic type inference: integer + double -> double
#' unvecvec(vv)
#'
#' # Force a specific output type
#' unvecvec(vv, ptype = character())
#'
#' @export
unvecvec <- function(x, ptype = NULL) {
  if ((len <- length(x)) == 0L) {
    return(unlist(x@x))
  }

  # Cast mixed vector types to common type
  if (is.null(ptype)) {
    ptype <- vec_ptype_common(!!!x@x) %||% logical()
    # Use safe casting from vctrs
    vec_cast_unsafe <- vec_cast
  }
  x@x <- lapply(x@x, vec_cast_unsafe, to = ptype)

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
    return(S7_class(x)(x = list(), i = idx))
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

  S7_class(..1)(
    x = unlist(x, recursive = FALSE),
    i = unlist(i, recursive = FALSE)
  )
}
