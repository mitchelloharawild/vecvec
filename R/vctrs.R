# vctrs compatibility methods

# proxy/restore
# This is inefficient but seemingly required for vctrs machinery
method(vec_proxy, class_vecvec) <- function(x, ...) {
  data_frame(x = list(x@x), l = sum(lengths(x@x)), i = S7_data(x))
}
method(vec_restore, class_vecvec) <- function(x, to, ...) {
  if (is_vecvec(x)) return(x)
  if (vec_size(x) == 0L) {
    return(S7_class(to)())
  }

  # Fast path for vecvec without merging - just update the indices
  if(identical(sum(slot_len <- lengths(to@x)), slot_grp <- unique(x$l))) {
    S7_data(to) <- x$i
    # Invoke pruning of slots
    out <- to[seq_along(to)]
    return(out)
  }
  
  # Identify groups of vectors
  # TODO - this destroys altrep
  grp <- data_frame(key = unique(x$x))
  grp$loc <- split(seq_along(x$i), match(x$x, grp$key))

  # val <- x$x[vapply(grp$loc, `[[`, integer(1), 1L)]
  # vctrs seems to drop list() to NULL internally somewhere? - fix it
  val <- lapply(grp$key, function(x) if (is.null(x)) list() else x)

  # Index offsets for each grp
  len <- c(
    0L,
    cumsum(vapply(val[-length(val)], function(x) sum(lengths(x)), integer(1)))
  )
  idx <- lapply(vec_seq_along(grp), function(i) x$i[grp$loc[[i]]] + len[i])

  # Restore the vecvec S7 object
  S7_class(to)(
    x = unlist(val, recursive = FALSE),
    i = unlist(idx, recursive = FALSE)
  )
}

# Comparison proxies
method(vec_proxy_equal, class_vecvec) <- function(x, ...) {
  # Build the equality proxy slot-wise on same-ptype slots
  n <- vec_size(x)
  idx <- S7_data(x)

  # No storage at all -> every element is necessarily an unassigned index.
  if (length(x@x) == 0L) {
    return(data_frame(.group = rep(NA_integer_, n)))
  }

  # Map each stored position to (slot, position-within-slot).
  slot_len <- lengths(x@x)
  slot_bounds <- c(0L, cumsum(slot_len))
  slot <- findInterval(idx, slot_bounds, left.open = TRUE)
  local_pos <- idx - slot_bounds[slot]

  # Group slots sharing a common ptype, as `duplicated()` does - only slots
  # with identical ptypes can ever compare equal to one another.
  ptypes <- lapply(x@x, `[`, 0L)
  uniq_ptypes <- unique(ptypes)
  loc <- lapply(
    uniq_ptypes,
    function(k) which(vapply(ptypes, identical, logical(1), k))
  )
  slot_group <- integer(length(ptypes))
  for (g in seq_along(loc)) slot_group[loc[[g]]] <- g

  # Per-element group id; unassigned indices (NA) get the reserved NA group.
  elem_group <- ifelse(is.na(slot), NA_integer_, slot_group[slot])

  result <- data_frame(.group = elem_group)
  for (g in seq_along(loc)) {
    member_slots <- loc[[g]]
    rows <- which(elem_group == g)
    if (length(rows) == 0L) next

    # Compute the proxy once for the whole ptype group
    proxy_g <- vec_proxy_equal(vec_c(!!!x@x[member_slots]), ...)
    if (!is.data.frame(proxy_g)) proxy_g <- data_frame(x = proxy_g)

    # Position of each member row within the group's concatenation.
    member_lens <- slot_len[member_slots]
    offsets <- c(0L, cumsum(member_lens))
    member_rank <- match(slot[rows], member_slots)
    grp_pos <- offsets[member_rank] + local_pos[rows]

    # Scatter the small per-group proxy into `n` rows via one vectorized
    # slice, leaving non-member rows as NA placeholders.
    filled <- vec_init(proxy_g, n)
    filled <- vec_assign(filled, rows, vec_slice(proxy_g, grp_pos))
    names(filled) <- paste0("g", g, "_", names(filled))

    result <- vec_cbind(result, filled)
  }

  result
}
method(vec_proxy_compare, class_vecvec) <- function(x, ...) {
  xtfrm(x, ...)
}

# prototypes
vec_ptype2_vecvec <- function(x, y, ...) {
  if (!is_vecvec(x)) {
    x <- vecvec(x)
  }
  if (!is_vecvec(y)) {
    y <- vecvec(y)
  }

  x <- c(x, y)
  S7_data(x) <- integer()
  x
}
#' @export
`vec_ptype2.vecvec::vecvec.vecvec::vecvec` <- vec_ptype2_vecvec

# casting

#' Cast methods for vecvec types
#'
#' These functions are the underlying handlers for [vctrs::vec_cast()] when
#' casting to or from a `vecvec` type. They are not called directly; instead,
#' they are registered dynamically in [.onLoad()] as S3 methods covering every
#' `vec_cast.*` generic exported by **vctrs**.
#'
#' @param x The object to cast.
#' @param to The target ptype.
#' @param ... Passed on to inner [vctrs::vec_cast()] calls.
#'
#' @return
#' * `vec_cast_to_vecvec()` returns a `vecvec` object. Each element of `x` is
#'   cast to the ptype of the corresponding group in `to@x`.
#' * `vec_cast_from_vecvec()` returns an object of the type described by `to`,
#'   produced by [unvecvec()].
#'
#' @seealso [unvecvec()], [vctrs::vec_cast()]
#' @noRd
vec_cast_to_vecvec <- function(x, to, ...) {
  # If input and ptype have incompatible structure, produce flat vecvec type
  if (length(x) != length(to)) return(S7_class(to)(list(x)))
  
  # TODO - handle replicated indices
  if (anyDuplicated(S7_data(to))) {
    stop("Casting to vecvec with duplicated indices is not supported.", call. = FALSE)
  }

  # Match index positions and vec_cast the individual vectors
  idx <- S7_data(to)
  len <- c(0L, cumsum(lengths(to@x[-length(to@x)])))
  pos <- findInterval(idx, len, left.open = TRUE)
  loc <- vec_split(x, pos)
  to@x <- .mapply(
    function(i, val) vec_cast(val, to@x[[i]], ...),
    list(loc$key, loc$val), NULL
  )

  to
}
vec_cast_from_vecvec <- function(x, to, ...) {
  unvecvec(x, ptype = to)
}
#' @method vec_cast vecvec::vecvec
#' @export
`vec_cast.vecvec::vecvec` <- function(x, to, ...) {
  UseMethod("vec_cast.vecvec::vecvec", to)
}
# labels
method(vec_ptype_full, class_vecvec) <- function(x, ...) {
  if (length(x@x) != 1L) "vecvec" else paste0(vec_ptype_full(x@x[[1L]]), "*")
}
method(vec_ptype_abbr, class_vecvec) <- function(x, ...) {
  if (length(x@x) != 1L) "vecvec" else paste0(vec_ptype_abbr(x@x[[1L]]), "*")
}
