# vctrs compatibility methods

# proxy/restore
# Each row of the proxy holds the (shared) list of stored vectors of the vecvec
# it came from, and its index into them. Slicing and combining proxies keeps
# the rows with their stored vectors, so restoring needs no information from
# `to` other than its class.
method(vec_proxy, class_vecvec) <- function(x, ...) {
  i <- vv_index(x)
  attributes(i) <- NULL
  data_frame(x = list(vv_slots(x)), i = i)
}
method(vec_restore, class_vecvec) <- function(x, to, ...) {
  if (is_vecvec(x)) return(x)
  # vctrs treats all vecvecs of a class as one type, so a prototype (as from
  # vec_ptype()) doesn't depend on the types a vecvec holds.
  if (vec_size(x) == 0L) {
    return(S7_class(to)())
  }

  # Group rows by the stored vectors they index into (the same object, so
  # cheap to compare), keeping groups in order of first appearance.
  grp_id <- .Call("vecvec_group_identity", x$x, PACKAGE = "vecvec")
  first <- which(!duplicated(grp_id))
  # Rows initialised by vctrs (e.g. with vec_init()) hold NULL
  val <- lapply(x$x[first], function(s) if (is.null(s)) list() else s)

  idx <- x$i
  if (length(val) > 1L) {
    # Offset each group's indices past the stored vectors of earlier groups
    len <- vapply(val, function(s) sum(list_sizes(s)), integer(1L))
    idx <- idx + c(0L, cumsum(len))[grp_id]
  }

  # Rows that are all missing (e.g. from vec_init()) keep the types of `to`
  slots <- unlist(val, recursive = FALSE)
  if (length(slots) == 0L) slots <- vv_slots(to)

  vv_rebuild(to, slots, idx)
}

# Comparison proxies
method(vec_proxy_equal, class_vecvec) <- function(x, ...) {
  # Build the equality proxy slot-wise on same-ptype slots
  n <- vec_size(x)
  idx <- vv_index(x)

  # No storage at all -> every element is necessarily an unassigned index.
  if (length(x@x) == 0L) {
    return(data_frame(.group = rep(NA_integer_, n)))
  }

  # Map each stored position to (slot, position-within-slot).
  slot_len <- list_sizes(x@x)
  at <- vecvec_locate(x@x, idx)
  slot <- at$slot
  local_pos <- at$within

  # Group slots sharing a common ptype, as `duplicated()` does - only slots
  # with identical ptypes can ever compare equal to one another.
  ptypes <- lapply(x@x, vec_slice, 0L)
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
  vv_index(x) <- integer()
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

  # Nothing to cast into
  if (length(to@x) == 0L) return(to)

  idx <- vv_index(to)
  pos <- vecvec_locate(to@x, idx)$slot

  if (!anyDuplicated(idx)) {
    # Match index positions and vec_cast the individual vectors
    loc <- vec_split(x, pos)
    to@x <- .mapply(
      function(i, val) vec_cast(val, to@x[[i]], ...),
      list(loc$key, loc$val), NULL
    )

    return(to)
  }

  # `to`'s stored positions are duplicated, so several logical elements can
  # share a single stored value. `x` may supply different values for those
  # common elements, so we can't simply overwrite in place.
  loc <- vec_split(seq_along(x), pos)
  new_x <- vector("list", nrow(loc))
  new_idx <- integer(length(x))
  offset <- 0L
  for (g in seq_len(nrow(loc))) {
    positions <- loc$val[[g]]
    new_x[[g]] <- vec_cast(vec_slice(x, positions), to@x[[loc$key[[g]]]], ...)
    new_idx[positions] <- offset + seq_along(positions)
    offset <- offset + length(positions)
  }

  S7_class(to)(x = new_x, i = new_idx)
}
# Cast between two vecvecs of the same class. A vecvec holds any mix of slot
# types, so any vecvec of the class already is a valid value of its type.
vec_cast_same_vecvec <- function(x, to, ...) x
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
