# vctrs compatibility methods

# proxy/restore
#
# vctrs treats a single-type vecvec as a vector of its type, and a mixed-type
# vecvec as a vector of any types (see vec_ptype2_vecvec()), and the two have
# different proxies:
#
# * single-type: its data. vctrs slices and combines this as it would the
#   plain vector, and restoring puts back the attributes of `to`, which has
#   the same type.
# * mixed-type: a data frame. Each row holds the (shared) list of stored
#   vectors of the vecvec it came from, and its index into them. Slicing and
#   combining keeps the rows with their stored vectors, so restoring needs only
#   the class of `to`. (A mixed-type `to` from vctrs is usually an empty
#   prototype, so the rows can't refer to it instead.)
#
# vctrs only combines the proxies of vectors of one type (it casts them to
# their common type first), so casts between vecvecs give the form of `to`
# (see vecvec_cast()). A single-type vecvec is held as an index when it's cast
# to a mixed-type vecvec.
#
# Subclasses opt in to single-type vctrs types with vecvec_register()
# (`single_type = TRUE`), as their own casts and prototypes must then respect
# them. Vecvecs of other classes are all one vctrs type, with the mixed-type
# proxy.
vv_single_type_classes <- new.env(parent = emptyenv())

# Does vctrs treat vecvecs of the class of `x` (a vecvec) as vectors of their
# type when they're single-type?
vv_single_type_class <- function(x) {
  isTRUE(vv_single_type_classes[[class(x)[[1L]]]])
}

# Is `x` a single-type vecvec that vctrs treats as a vector of its type?
vv_single_type_vctrs <- function(x) vv_is_typed(x) && vv_single_type_class(x)

method(vec_proxy, class_vecvec) <- function(x, ...) {
  if (vv_single_type_vctrs(x)) {
    attributes(x) <- NULL
    return(x)
  }
  i <- vv_index(x)
  if (!is.null(attributes(i))) attributes(i) <- NULL
  new_data_frame(list(x = vv_rep_list(vv_slots(x), length(i)), i = i))
}
method(vec_restore, class_vecvec) <- function(x, to, ...) {
  if (is_vecvec(x)) return(x)
  # The values of a single-type vecvec
  if (!is.data.frame(x)) {
    return(vv_with_data(to, x))
  }
  # A mixed-type prototype (as from vec_ptype()) doesn't depend on the types a
  # vecvec holds.
  if (vec_size(x) == 0L) {
    return(S7_class(to)())
  }

  # Group rows by the stored vectors they index into (the same object, so
  # cheap to compare), keeping groups in order of first appearance.
  grp <- .Call("vecvec_group_identity", x$x, PACKAGE = "vecvec")
  first <- attr(grp, "first", exact = TRUE)
  # Rows initialised by vctrs (e.g. with vec_init()) hold NULL
  val <- lapply(x$x[first], function(s) if (is.null(s)) list() else s)

  idx <- x$i
  if (length(val) > 1L) {
    # Offset each group's indices past the stored vectors of earlier groups
    len <- vapply(val, function(s) sum(list_sizes(s)), integer(1L))
    attributes(grp) <- NULL
    idx <- idx + c(0L, cumsum(len))[grp]
  }

  # Rows that are all missing (e.g. from vec_init()) keep the types of `to`
  slots <- unlist(val, recursive = FALSE)
  if (length(slots) == 0L) slots <- vv_slots(to)

  vv_rebuild(to, slots, idx)
}

# Comparison proxies
#
# vctrs compares the equality proxies of two mixed-type vecvecs (as in
# vec_match(), vec_in() or joins) column by column, so every mixed-type vecvec
# has the same proxy layout, whatever types it holds. Each element is
# described by:
#
# * type: the id of its type (see vv_type_id()).
# * num: its value, if the equality proxy of its type is logical, integer or
#   double.
# * chr: its value, if the equality proxy of its type is character.
# * obj: its value otherwise (e.g. complex values, or rows of a data frame
#   proxy, such as of a record).
#
# Columns an element doesn't use hold a fixed, non-missing filler, so equal
# elements are equal even with `na_equal = FALSE`. Missing elements (missing
# values, and missing indices) are missing in every column, as vctrs requires
# to detect them, so missing values of different types are equal.
method(vec_proxy_equal, class_vecvec) <- function(x, ...) {
  # A single-type vecvec is compared with vecvecs of its type (vctrs casts to
  # the common type first), so it compares as its type.
  if (vv_single_type_vctrs(x)) return(vec_proxy_equal(vv_typed_slot(x), ...))

  n <- vec_size(x)
  type <- rep_len(NA_integer_, n)
  num <- rep_len(0, n)
  chr <- rep_len("", n)
  obj <- vv_rep_list(FALSE, n)

  slots <- vv_slots(x)
  at <- vecvec_locate(slots, vv_index(x))
  rows <- split(seq_len(n), factor(at$slot, seq_along(slots)))
  # Missing indices
  miss <- which(is.na(at$slot))
  if (length(miss)) {
    num[miss] <- NA_real_
    chr[miss] <- NA_character_
    obj[miss] <- list(NULL)
  }

  for (s in seq_along(slots)) {
    r <- rows[[s]]
    if (length(r) == 0L) next
    p <- vec_slice(vec_proxy_equal(slots[[s]], ...), at$within[r])

    if (is.data.frame(p) || !typeof(p) %in% c("logical", "integer", "double", "character")) {
      obj[r] <- vec_chop(p)
    } else {
      v <- unclass(p)
      attributes(v) <- NULL
      if (is.character(v)) chr[r] <- v else num[r] <- as.double(v)
    }
    type[r] <- vv_type_id(vv_slot_key(slots[[s]]))

    miss <- which(vec_detect_missing(p))
    if (length(miss)) {
      miss <- r[miss]
      type[miss] <- NA_integer_
      num[miss] <- NA_real_
      chr[miss] <- NA_character_
      obj[miss] <- list(NULL)
    }
  }

  new_data_frame(list(type = type, num = num, chr = chr, obj = obj), n = n)
}

# A list of length `n` whose elements are all `x`.
vv_rep_list <- function(x, n) .Call("vecvec_rep_list", x, n, PACKAGE = "vecvec")

# Types are numbered in the order they're first seen in the session. Types are
# the same when their prototypes are identical (as when grouping slots, see
# vv_normalise()), which hashes can't tell reliably (the functions of an S7
# class serialize differently once byte-compiled).
vv_types <- new.env(parent = emptyenv())
vv_types$ptypes <- list()
vv_types$labels <- character()

vv_type_id <- function(ptype) {
  label <- paste(c(typeof(ptype), class(ptype)), collapse = "/")
  for (i in which(vv_types$labels == label)) {
    if (vv_same_type(vv_types$ptypes[[i]], ptype)) return(i)
  }
  id <- length(vv_types$ptypes) + 1L
  vv_types$ptypes[[id]] <- ptype
  vv_types$labels[[id]] <- label
  id
}

method(vec_proxy_compare, class_vecvec) <- function(x, ...) {
  # A single-type vecvec compares as its type, so its proxy can be compared
  # with that of another vecvec of the type.
  if (vv_single_type_vctrs(x)) return(vec_proxy_compare(vv_typed_slot(x), ...))
  vv_proxy_compare_mixed(x, ...)
}

# prototypes
vec_ptype2_vecvec <- function(x, y, ...) {
  # Combine prototypes rather than values: the prototype of a single-type
  # vecvec keeps its type, which the common type keeps if it's the same.
  px <- if (is_vecvec(x)) vv_ptype(x) else vecvec(vec_ptype(x))
  py <- if (is_vecvec(y)) vv_ptype(y) else vecvec(vec_ptype(y))
  out <- c(px, py)
  vv_index(out) <- integer()
  vv_ptype2_form(out, px, py)
}

# The common type of vecvecs is single-type only when all of them have that
# type: a mixed-type prototype holds no types, so it would otherwise be lost
# when combined. `out` is the common type found by a vec_ptype2() method
# (from vecvec or a subclass) for `x` and `y`, which needn't be vecvecs.
vv_ptype2_form <- function(out, x, y) {
  if (!is_vecvec(out) || !vv_single_type_vctrs(out)) return(out)
  p <- vv_typed_ptype(out)
  for (v in list(x, y)) {
    if (is_vecvec(v) && !(vv_is_typed(v) && vv_same_type(vv_typed_ptype(v), p))) {
      return(vv_rebuild(out, list(), integer()))
    }
  }
  out
}

# A zero-length vecvec like `x`, without copying its values.
vv_ptype <- function(x) {
  slots <- vv_slots(x)
  if (vv_is_typed(x)) slots <- list(vec_slice(slots[[1L]], 0L))
  vv_rebuild(x, slots, integer())
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
  vecvec_cast(vec_cast_to_vecvec_impl(x, to, ...), to, ...)
}
vec_cast_to_vecvec_impl <- function(x, to, ...) {
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
# Cast between two vecvecs of the same class
vec_cast_same_vecvec <- function(x, to, ...) vecvec_cast(x, to, ...)
vec_cast_from_vecvec <- function(x, to, ...) {
  unvecvec(x, ptype = to)
}

#' Cast a vecvec to the form of another
#'
#' vctrs treats a single-type vecvec (whose elements all have one type) as a
#' vector of that type, and a mixed-type vecvec as a vector of any types.
#' `vecvec_cast()` casts `x` to the vctrs type of `to`, a vecvec of the same
#' class: the values of `x` are cast to the type of a single-type `to` (with
#' [vctrs::vec_cast()], so this errors for values that can't be cast), and are
#' kept as they are for a mixed-type `to`.
#'
#' This applies to [vecvec()] and to subclasses registered with
#' `vecvec_register(single_type = TRUE)`. All vecvecs of other subclasses are
#' one vctrs type, and `vecvec_cast()` returns `x`.
#'
#' A `vec_cast()` method between two vecvecs of a subclass registered with
#' `single_type = TRUE` must return the result of `vecvec_cast()`, as vctrs
#' needs the result to have the form of `to` when combining vectors.
#'
#' @param x,to Vecvecs of the same class.
#' @param ... Passed on to [vctrs::vec_cast()].
#'
#' @return A vecvec of the class of `x`, of the vctrs type of `to`.
#'
#' @examples
#' # To a single type
#' vecvec_cast(vecvec(1:3), vecvec(0.5))
#'
#' # Any types
#' vecvec_cast(vecvec(1:3), vecvec(1, "a"))
#' @export
vecvec_cast <- function(x, to, ...) {
  # All vecvecs of the class are one vctrs type
  if (!vv_single_type_class(to)) return(x)
  if (!vv_is_typed(to)) return(vv_as_indexed(x))

  ptype <- vec_slice(vv_typed_slot(to), 0L)
  if (vv_is_typed(x) && vv_same_type(vv_typed_ptype(x), vv_typed_ptype(to))) {
    return(x)
  }
  slots <- lapply(vv_slots(x), function(s) vec_cast(s, ptype, ...))
  # Missing values only
  if (length(slots) == 0L) slots <- list(ptype)
  vv_rebuild(x, slots, vv_index(x))
}

# A vecvec held as an index (the mixed-type form), even if it has a single
# type.
vv_as_indexed <- function(x) {
  if (!vv_is_typed(x)) return(x)
  at <- attributes(x)
  at[[vv_ptype_attr]] <- NULL
  at[[vv_slots_attr]] <- list(vv_typed_slot(x))
  i <- seq_len(length(x))
  attributes(i) <- at
  i
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
