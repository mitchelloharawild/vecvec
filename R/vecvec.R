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
  n_vecs <- length(x@x)

  # Cast mixed vector types to common type
  if (!is.null(ptype)) {
    x@x <- lapply(x@x, vec_cast, to = ptype)
  } else if (n_vecs > 1L) {
    ptype <- vec_cast_common(!!!x@x)
  }

  # Apply ordering to attribute vectors
  vec_slice(vec_c(!!!x@x), x@i)
}

# Display methods
method(print, class_vecvec) <- function(x, ...) {
  vctrs::obj_print(x, ...)
}
method(format, class_vecvec) <- function(x, ...) {
  vec_c(!!!lapply(x@x, format, ...), .ptype = character())[x@i]
}


restore_class <- function(x) {
  setdiff(class(x), c("vecvec", "vctrs_rcrd", "vctrs_vctr"))
}

#' @export
vec_proxy.vecvec <- function(x, ...) {
  # Somewhat inefficient, copy pointers to vectors by row
  vctrs::data_frame(
    x = field(x, "x"), v = attr(x, "v")[field(x, "i")]
  )
}

#' @export
vec_proxy_equal.vecvec <- function(x, ...) {
  # TODO - implement using a faster method (e.g. hashing)
  n_vecs <- length(attr(x, "v"))
  i_offset <- cumsum(c(0, lengths(attr(x, "v"))[-n_vecs]))
  list_unchop(lapply(attr(x, "v"), as.list))[i_offset[field(x, "i")] + field(x, "x")]
}

#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  # TODO: combine common groups
  if(vec_is_empty(x)) return(new_vecvec())
  v_grp <- vec_group_loc(x$v)
  na_vec <- vapply(v_grp$key, is.null, logical(1L))
  i_loc <- cumsum(!na_vec)
  i_loc[na_vec] <- NA_integer_

  res <- new_vecvec(
    x = v_grp$key[!na_vec],
    loc = list(
      i = rep(i_loc, lengths(v_grp$loc))[order(list_unchop(v_grp$loc))],
      x = x$x
    ),
    class = restore_class(to)
  )
  return(vecvec_compress(res))
}

vec_cast_from_vecvec <- function(x, to, ...) {
  out <- lapply(attr(x, "v"), vec_cast, to = to, ...)
  list_unchop(.mapply(function(i, x) out[[i]][[x]], new_data_frame(x), NULL))
}

vec_cast_to_vecvec <- function(x, to, ...) {
  new_vecvec(list(x), class = restore_class(to))
}

#' @export
vec_ptype2.vecvec <- function(x, y, ...) {
  compat_class <- intersect(restore_class(x), restore_class(y))
  new_vecvec(class = compat_class)
}

#' @export
vec_ptype.vecvec <- function(x, ...) {
  new_vecvec(class = restore_class(x))
}

#' @export
vec_ptype2.vecvec.vecvec <- function(x, y, ...) {
  compat_class <- intersect(restore_class(x), restore_class(y))
  new_vecvec(class = compat_class)
# Indexing methods
method(`[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  len <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  pos <- findInterval(idx[!is.na(idx)], len, left.open = TRUE)
  grp <- vec_group_loc(pos)
  
  # TODO - if grp has only one group we can return a simpler vecvec type

  # Prune unused vectors
  x@x <- .mapply(
    function(key, loc) x@x[[key]][idx[loc] - len[key], drop = FALSE],
    grp, NULL
  )

  # Update indices
  idx[!is.na(pos)] <- unlist(grp$loc)
  x@i <- idx

  x
}
method(`[[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  len <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  pos <- findInterval(idx, len)
  x@x[[pos]][[idx - len[pos]]]
}

# Casting methods
method(convert, list(class_any, class_vecvec)) <- function(from, to, ...) {
  vecvec(from)
}
method(convert, 
  list(
    class_vecvec,
    new_union(
      class_vector,
      class_Date,
      class_POSIXct,
      class_POSIXlt,
      class_data.frame
    )
  )
) <- function(from, to, ...) unvecvec(from, ptype = to$constructor())

method(as.logical, class_vecvec) <- function(x) convert(x, class_logical)
method(as.raw, class_vecvec) <- function(x) convert(x, class_raw)
method(as.integer, class_vecvec) <- function(x) convert(x, class_integer)
# For some reason this method isn't being registered properly with S7::method<-
# method(as.double, class_vecvec) <- function(x) convert(x, class_double)
#' @export
#' @method as.double vecvec::vecvec
`as.double.vecvec::vecvec` <- function(x) convert(x, class_double)
method(as.complex, class_vecvec) <- function(x) convert(x, class_complex)
method(as.character, class_vecvec) <- function(x) convert(x, class_character)
method(as.list, class_vecvec) <- function(x) convert(x, class_list)
method(as.Date, class_vecvec) <- function(x) convert(x, class_Date)
method(as.POSIXct, class_vecvec) <- function(x) convert(x, class_POSIXct)
method(as.POSIXlt, class_vecvec) <- function(x) convert(x, class_POSIXlt)
method(as.data.frame, class_vecvec) <- function(x) convert(x, class_data.frame)

# Combining methods
method(c, class_vecvec) <- function(..., recursive = FALSE) {
  dots <- rlang::list2(...)
  is_vecvec <- vapply(dots, is_vecvec, logical(1L))
  dots[!is_vecvec] <- lapply(dots[!is_vecvec], vecvec)

  # TODO - reduce structure into a common vecvec
  i_offsets <- cumsum(vapply(
    dots[-length(dots)],
    function(x) sum(lengths(x@x)),
    integer(1L)
  ))

  x <- lapply(dots, function(x) x@x)
  i <- c(dots[[1L]]@i, .mapply(
    function(x, j) x@i + j,
    list(x = dots[-1L], j = i_offsets), NULL
  ))

  class_vecvec(
    x = unlist(x, recursive = FALSE),
    i = unlist(i, recursive = FALSE)
  )
}