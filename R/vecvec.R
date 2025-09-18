#' Construct a vector of vectors
#'
#' new_vecvec() constructs a new vector of vectors from a list of vectors. It is meant to be performant, and does not check the inputs for correctness in any way. It is only safe to use after a call to df_list(), which collects and validates the columns used to construct the data frame.
#'
#' @param x An unnamed list of arbitrary vectors.
#' @param loc A named list of value locations, with `i` identifying the vector index and `x` identifying the value index. By default, the order of appearance in `x` will be used.
#' @param class Name of subclass.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @examples
#' # Create a vecvec prototype
#' new_vecvec()
#'
#' # Construct a vecvec from a list of vectors
#' new_vecvec(list(letters, rnorm(10)))
#'
#' # Fully specify a vecvec with locations
#' new_vecvec(
#'   x = list(letters, rnorm(10)),
#'   loc = list(
#'     i = c(rep(1L, 3), rep(2L, 5), rep(1L, 23), rep(2L, 5)),
#'     x = c(1:3, 1:5, 26:4, 6:10)
#'   )
#' )
#'
#' @export
new_vecvec <- function(x = list(), loc = NULL, class = character()) {
  if(is.null(loc)) {
    size <- vapply(x, vec_size, integer(1L))
    loc <- if(identical(size, integer(0L))) {
      list(i = integer(), x = integer())
    } else {
      list(
        i = rep(seq_along(size), size),
        x = list_unchop(lapply(size, seq_len))
      )
    }
  }

  new_rcrd(
    loc,
    v = x,
    class = c(class, "vecvec")
  )
}

#' Create a new vector of vectors
#'
#' @param ... Vectors to combine into a single vector without type coercion.
#' @param class Name of subclass.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @seealso [unvecvec()] coerces the mixed-type vector into a single-typed
#' regular vector. [new_vecvec()] is a performant alternative that accepts a
#' list of vectors rather than `...` (suitable for R packages).
#'
#' @examples
#' vecvec(Sys.Date(), rnorm(3), letters)
#'
#' @export
vecvec <- function(..., class = character()) {
  vecvec_compress(new_vecvec(rlang::list2(...), class = class))
}

#' Convert a vecvec object into its underlying vector type
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x A vecvec to unvecvec (convert to its underlying vector type)
#' @inheritParams vctrs::list_unchop
#'
#' @return A simple vector, all containing the same type of data.
#'
#' @export
unvecvec <- function(x, ..., ptype = NULL) {
  n_vecs <- length(attr(x, "v"))

  # Cast mixed vector types to common type
  if(n_vecs > 1) {
    attr(x, "v") <- vec_cast_common(!!!attr(x, "v"), .to = ptype)
  }

  # Apply ordering to attribute vectors
  i_offset <- cumsum(c(0, lengths(attr(x, "v"))[-n_vecs]))
  list_unchop(attr(x, "v"))[i_offset[field(x, "i")] + field(x, "x")]
}

#' @export
format.vecvec <- function(x, ...) {
  out <- lapply(attr(x, "v"), format, ...)
  unlist(
    .mapply(function(i, x){
      if(is.na(i)) NA_character_ else out[[i]][[x]]
    }, list(field(x, "i"), field(x, "x")), NULL)
  )
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
  vec_unchop(.mapply(function(i, x) out[[i]][[x]], new_data_frame(x), NULL))
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
}


#' @export
vec_cast.vecvec.vecvec <- function(x, to, ...) {
  x
}
