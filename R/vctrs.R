# vctrs compatibility methods

# proxy/restore
# This is inefficient but seemingly required for vctrs machinery
method(vec_proxy, class_vecvec) <- function(x, ...) {
  data_frame(x = list(x@x), i = S7_data(x))
}
method(vec_restore, class_vecvec) <- function(x, to, ...) {
  if (vec_size(x) == 0L) {
    return(S7_class(to)())
  }

  # Identify groups of vectors
  # TODO - this destroys altrep
  grp <- vec_group_loc(x$x)

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
  # This is inefficient, but seems necessary for vctrs machinery.
  # Directly using `==` is faster as it applies on overlapping vctrs directly.
  data_frame(x = as.list(x), na = ifelse(is.na(x), NA, FALSE))
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
