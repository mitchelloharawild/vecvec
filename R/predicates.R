method(is.finite, class_vecvec) <- vecvec_apply_fn(is.finite, ptype = logical())
method(is.infinite, class_vecvec) <- vecvec_apply_fn(is.infinite, ptype = logical())
method(is.nan, class_vecvec) <- vecvec_apply_fn(is.nan, ptype = logical())

method(is.na, class_vecvec) <- function(x) {
  # A single-type vecvec: the type's own method, keeping dim, dimnames and names
  if (vv_is_typed(x)) {
    res <- is.na(vv_typed_slot(x))
    at <- attributes(vv_data(x))
    if (!is.null(at)) attributes(res) <- at
    return(res)
  }
  # Missing values in vecvec indices or values are both considered NA.
  is.na(vv_index(x)) | unvecvec(vecvec_apply(x, is.na), ptype = logical())
}
method(anyNA, class_vecvec) <- function(x, recursive = FALSE) {
  if (anyNA(vv_index(x))) return(TRUE)
  
  for (v in x@x) {
    if (anyNA(v, recursive = recursive)) return(TRUE)
  }

  FALSE
}
method(na.fail, class_vecvec) <- function(object, ...) {
  if (anyNA(object)) {
    cli::cli_abort(
      c(
        "Missing values in object of class {.cls vecvec}.",
        "i" = "Use {.fn na.omit} or {.fn na.exclude} to remove missing values."
      ),
      call = NULL
    )
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

# Canonical id for each element of a vecvec: the stored position of the first
# stored value equal to it. Equality is only checked within slots sharing a
# common ptype (as for `vec_proxy_equal()`), and NA indices are kept as NA so
# they match each other but not stored missing values.
#
# Unlike comparing the stored values directly, this also identifies repeated
# indices pointing at the same stored value as duplicates.
#
# @return A list with `id`, an integer vector the same length as `x`, and
#   `incomparables`, the ids to pass on to base `duplicated()`.
vecvec_dup_id <- function(x, incomparables = FALSE) {
  # Find common vector types
  grp <- vv_group_identical(lapply(x@x, vv_slot_key))
  loc <- split(seq_along(grp), grp)

  slot_len <- list_sizes(x@x)
  offsets <- c(0L, cumsum(slot_len))
  canon <- integer(sum(slot_len))
  inc <- logical(sum(slot_len))
  for (i in loc) {
    # Stored positions of this group's values, in concatenation order
    pos <- unlist(lapply(i, function(s) offsets[s] + seq_len(slot_len[s])))
    vec <- vec_c(!!!x@x[i])
    grp <- vec_group_id(vec)
    canon[pos] <- pos[match(grp, grp)]
    if (!isFALSE(incomparables)) {
      inc[pos] <- match(vec, incomparables, 0L) > 0L
    }
  }

  id <- canon[vv_index(x)]
  inc_id <- unique(canon[inc])
  # Missing indices are treated as missing values
  if (!isFALSE(incomparables) && anyNA(incomparables)) {
    inc_id <- c(inc_id, NA_integer_)
  }
  list(id = id, incomparables = if (length(inc_id)) inc_id else FALSE)
}

method(duplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  # A single-type vecvec: its values are stored once each, in order
  if (vv_is_typed(x)) {
    return(duplicated(vv_typed_slot(x), incomparables = incomparables, ...))
  }
  id <- vecvec_dup_id(x, incomparables)
  duplicated(id$id, incomparables = id$incomparables, ...)
}

method(anyDuplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  if (vv_is_typed(x)) {
    return(anyDuplicated(vv_typed_slot(x), incomparables = incomparables, ...))
  }
  id <- vecvec_dup_id(x, incomparables)
  anyDuplicated(id$id, incomparables = id$incomparables, ...)
}

# The values match() compares (its mtfrm() method). A single-type vecvec
# compares as its type, as the plain vector does. A mixed-type vecvec compares
# as the common type of its types (e.g. integers and doubles as doubles), and
# otherwise as strings, as match() does for plain vectors of different types
# (e.g. match(1L, "1") is 1).
vv_mtfrm <- function(x) {
  values <- if (vv_is_typed(x)) {
    vv_typed_slot(x)
  } else if (!is.null(vv_common_ptype(vv_slots(x)))) {
    unvecvec(x)
  } else {
    return(as.character(x))
  }
  # match() only calls mtfrm() for objects (mtfrm() of a bare vector is
  # as.character())
  if (is.object(values)) mtfrm(values) else values
}
