#' Compute overlap segments between two integer run-length vectors
#'
#' @description
#' Given two integer vectors `a` and `b` representing run lengths
#' along a common 1D axis (e.g. segments of contiguous positions),
#' this function computes the lengths of consecutive overlapping segments
#' between them. It assumes that `a` and `b` are positive integers and that
#' their sums are equal (i.e. lengths along same-sized vecvecs)
#'
#' Conceptually, you"“walk" along the sums of `a` and `b` simultaneously,
#' taking at each step the minimum of the remaining length in the current
#' element of `a` and `b`, and recording that as one overlap segment.
#'
#' @param a Integer vector of positive run lengths.
#' @param b Integer vector of positive run lengths.
#'
#' @return
#' An integer vector giving the lengths of consecutive overlapping segments.
#'
#' @details
#' This is an \eqn{O(length(a) + length(b))} algorithm using a single pass
#' through both vectors with constant additional memory, apart from the
#' preallocated output.
#'
#' @examples
#' a <- c(4L, 8L, 3L)
#' b <- c(3L, 9L, 3L)
#'
#' overlap_indices(a, b)
#' # [1] 3 1 8 3
#'
#' @noRd
overlap_indices <- function(a, b) {
  n_a <- length(a)
  n_b <- length(b)

  # Upper bound (very loose but safe)
  len <- i_a <- i_b <- integer(n_a + n_b - 1L)

  k <- 0L

  i <- 1L
  j <- 1L
  ra <- a[i]
  rb <- b[j]

  while (i <= n_a && j <= n_b) {
    x <- if (ra < rb) ra else rb
    k <- k + 1L
    len[k] <- x
    i_a[k] <- i
    i_b[k] <- j

    ra <- ra - x
    rb <- rb - x

    if (ra == 0L) {
      i <- i + 1L
      if (i <= n_a) ra <- a[i]
    }
    if (rb == 0L) {
      j <- j + 1L
      if (j <= n_b) rb <- b[j]
    }
  }

  # Trim to actual used length
  list(
    len = len[seq_len(k)],
    idx = list(
      i_a = i_a[seq_len(k)],
      i_b = i_b[seq_len(k)]
    )
  )
}

# Map global positions (1-based offsets into the concatenation of `slots`)
# to their (slot, within-slot) location.
#
# This is the arithmetic shared by every operation that needs to know which
# element of a vecvec's @x a stored value lives in - indexing, casting,
# equality proxies, duplicate detection, comparisons, etc.
#
# @param slots A list of vectors, e.g. `x@x`, or a subset of it.
# @param pos Integer vector of 1-based positions into the concatenation of
#   `slots`. May contain `NA`, which map to `NA` slot/within-slot values.
#
# @return A list with `slot` (which element of `slots` each position falls
#   in) and `within` (the corresponding 1-based position inside that slot).
vecvec_locate <- function(slots, pos) {
  bounds <- c(0L, cumsum(list_sizes(slots)))
  slot <- findInterval(pos, bounds, left.open = TRUE)
  list(slot = slot, within = pos - bounds[slot])
}

# Encode a pair of per-object slot indices into a single grouping key, such
# that equal keys imply an equal (slot_x, slot_y) pair. Used to group
# elements of two vecvecs that draw from the same pair of underlying slots
# regardless of position (e.g. for `all.equal()`, where groups need not be
# contiguous). See `vecvec_align()` below for the contiguous-run variant used
# where output order/structure must be preserved (e.g. `Ops`).
#
# @param slot_x,slot_y Integer vectors of slot indices (as returned by
#   `vecvec_locate()$slot`), the same length.
# @param n_y The number of slots in the second object (e.g. `length(y@x)`).
#
# @return An integer vector the same length as `slot_x`/`slot_y`.
vecvec_pair_key <- function(slot_x, slot_y, n_y) {
  (slot_x - 1L) * (n_y + 1L) + slot_y
}

# Align multiple same-length vecvec objects to a common element-position
# grouping: locates each argument's underlying (slot, within-slot) position
# for every element (via `vecvec_locate()`), then partitions positions into
# groups that draw from the same tuple of underlying slots across all
# arguments (not necessarily contiguous).
#
# A run boundary occurs wherever *any* argument's slot index changes between
# consecutive positions - equivalent to encoding the tuple of slot indices as
# a single key and detecting key changes (as when there were only two
# arguments), but generalises to any number of arguments without needing to
# construct the key.
#
# This is the shared batching strategy behind operations that need to call a
# vectorised function once per distinct combination of underlying storage
# vectors rather than once per element - e.g. binary `Ops` on vecvec, or
# element extraction in `vecvec_mapply()`.
#
# @param args A list of vecvec objects, of sizes that recycle to a common size
#   (their indices are recycled with `vec_recycle_common()`, so the values of
#   a recycled single-type vecvec aren't expanded).
#
# @return A list with:
#   - `slot`, `within`: lists (parallel to `args`) of per-argument slot /
#     within-slot-position integer vectors, one entry per element position.
#   - `groups`: positions partitioned into contiguous runs that share a fixed
#     tuple of underlying slots, as returned by `split()`.
vecvec_align <- function(args) {
  index <- lapply(args, function(a) as.vector(vv_index(a)))
  index <- vec_recycle_common(!!!index)
  at <- .mapply(function(a, i) vecvec_locate(vv_slots(a), i), list(args, index), NULL)
  slot <- lapply(at, `[[`, "slot")
  within <- lapply(at, `[[`, "within")

  n <- length(slot[[1L]])
  if (n == 0L) {
    return(list(slot = slot, within = within, groups = list()))
  }
  # Group by the combination of slots, wherever the positions are. Positions
  # with a missing (NA) slot index in some arg share a group per NA pattern;
  # consumers skip any group whose first position has an NA slot.
  names(slot) <- paste0("s", seq_along(slot))
  groups <- vctrs::vec_group_loc(vctrs::new_data_frame(slot, n = n))$loc

  list(slot = unname(slot), within = within, groups = groups)
}

# Shim for `%||%`, which is only in base R from 4.4.0.
`%||%` <- function(x, y) if (is.null(x)) y else x

# The numeric reading of a stored vector, as `as.double()` gives it. The
# S3 method is looked up explicitly, as `as.double()` methods can be out of
# reach of internal dispatch: S7 registers `method(as.double, cls)` under the
# name `as.numeric.<cls>` (which `as.double()` never dispatches to), and a
# package whose namespace binds `as.double` (as `S7::method(as.double, cls) <-`
# does) registers its NAMESPACE `as.double` methods in its own S3 table.
vv_as_double <- function(x) {
  if (is.object(x) && !is.factor(x)) {
    for (cls in class(x)) {
      f <- utils::getS3method("as.double", cls, optional = TRUE) %||%
        utils::getS3method("as.numeric", cls, optional = TRUE)
      if (!is.null(f)) return(as.double(f(x)))
    }
  }
  as.double(x)
}
