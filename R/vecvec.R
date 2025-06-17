#' Construct a vector of vectors
#'
#' new_vecvec() constructs a new vector of vectors from a list of vectors. It is meant to be performant, and does not check the inputs for correctness in any way. It is only safe to use after a call to df_list(), which collects and validates the columns used to construct the data frame.
#'
#' @param x An unnamed list of arbitrary vectors.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @export
new_vecvec <- function(x = list()) {
  size <- lengths(x)
  out <- if(identical(size, integer(0L))) {
    list(i = integer(), x = integer())
  } else {
    list(
      i = rep(seq_along(size), size),
      x = unlist(lapply(size, seq_len))
    )
  }
  new_rcrd(
    out,
    v = x,
    class = "vecvec"
  )
}

#' Create a new vector of vectors
#'
#' @param ... Vectors to combine into a single vector without type coercion.
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
vecvec <- function(...) {
  new_vecvec(rlang::list2(...))
}

#' Convert a vecvec object into its underlying vector type
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x A vecvec to unvecvec (convert to its underlying vector type)
#' @inheritParams vctrs::list_unchop
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

# `[.vecvec` <- function(x, i) {
#   # TODO: Rework unique and match to be faster
#   v <- .mapply(
#     function(key, val) vec_slice(attr(x, "v")[[key]], unique(val)),
#     vec_split(.x <- field(x, "x")[i], .i <- field(x, "i")[i]),
#     NULL
#   )
#
#   new_rcrd(
#     list(i = match(.i, unique(.i)), x = match(.x, unique(.x))),
#     v = v,
#     class = "vecvec"
#   )
# }

#' @export
vec_proxy.vecvec <- function(x, ...) {
  # Somewhat inefficient, copy pointers to vectors by row
  vctrs::data_frame(
    x = field(x, "x"), v = attr(x, "v")[field(x, "i")]
  )
}

#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  # TODO: combine common groups
  if(vec_is_empty(x)) return(new_vecvec())
  v_grp <- vec_group_loc(x$v)
  na_vec <- vapply(v_grp$key, is.null, logical(1L))
  i_loc <- cumsum(!na_vec)
  i_loc[na_vec] <- NA_integer_

  return(
    vctrs::new_rcrd(
      list(
        i = rep(i_loc, lengths(v_grp$loc))[order(list_unchop(v_grp$loc))],
        x = x$x
      ),
      v = v_grp$key[!na_vec],
      class = "vecvec"
    )
  )
}

vec_cast_from_vecvec <- function(x, to, ...) {
  out <- lapply(attr(x, "v"), vec_cast, to = to, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], new_data_frame(x), NULL))
}

vec_cast_to_vecvec <- function(x, to, ...) {
  new_vecvec(list(x))
}

#' @export
vec_ptype2.vecvec <- function(x, y, ...) {
  new_vecvec()
}

#' @export
vec_ptype.vecvec <- function(x, ...) {
  new_vecvec()
}

#' @export
vec_ptype2.vecvec.vecvec <- function(x, y, ...) {
  new_vecvec()
}


#' @export
vec_cast.vecvec.vecvec <- function(x, to, ...) {
  x
}
