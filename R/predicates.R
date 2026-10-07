method(is.finite, class_vecvec) <- vecvec_apply_fn(is.finite, ptype = logical())
method(is.infinite, class_vecvec) <- vecvec_apply_fn(is.infinite, ptype = logical())
method(is.nan, class_vecvec) <- vecvec_apply_fn(is.nan, ptype = logical())

# Type-testing is.*() predicates (is.numeric(), is.character(), ...) normally
# check the type of the object they're given, not its elements. A vecvec 
# instead checks the types of its elements, so that a vecvec of numeric vectors 
# is considered numeric.
method(is.numeric, class_vecvec) <- function(x) {
  all(vapply(x@x, is.numeric, logical(1L)))
}

method(is.na, class_vecvec) <- function(x) {
  # Missing values in vecvec indices or values are both considered NA.
  is.na(S7_data(x)) | unvecvec(vecvec_apply(x, is.na), ptype = logical())
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
  ptypes <- lapply(x@x, `[`, 0L)
  loc <- lapply(
    unique(ptypes),
    function(k) which(vapply(ptypes, identical, logical(1), k))
  )

  slot_len <- lengths(x@x)
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

  id <- canon[S7_data(x)]
  inc_id <- unique(canon[inc])
  # Missing indices are treated as missing values
  if (!isFALSE(incomparables) && anyNA(incomparables)) {
    inc_id <- c(inc_id, NA_integer_)
  }
  list(id = id, incomparables = if (length(inc_id)) inc_id else FALSE)
}

method(duplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  id <- vecvec_dup_id(x, incomparables)
  duplicated(id$id, incomparables = id$incomparables, ...)
}

method(anyDuplicated, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  id <- vecvec_dup_id(x, incomparables)
  anyDuplicated(id$id, incomparables = id$incomparables, ...)
}
