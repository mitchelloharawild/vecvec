#' Create a new vector of vectors
#'
#' @param ... Individual vectors to combine into a vecvec
#'
#' @examples
#' new_vecvec(Sys.Date(), rnorm(3), letters)
#'
#' @export
new_vecvec <- function(...) {
  vecs <- rlang::list2(...)
  size <- lengths(vecs)
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
    v = vecs,
    class = "vecvec"
  )
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
  return(vctrs::data_frame(
    x = field(x, "x"), v = attr(x, "v")[field(x, "i")]
  ))


  # TODO: Bring attribute into table?
  # out <- list_unchop(.mapply(
  #   function(key, val) as.list(attr(x, "v")[[key]][val]),
  #   vec_split(field(x, "x"), field(x, "i")), NULL
  # ))
  # if(is.null(out)) return(list())
  # return(out)
  # new_data_frame(list(x = out))
  # structure(new_data_frame(x), v = attr(x, "v"))
}

#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  # TODO: combine common groups
  if(vec_is_empty(x)) return(new_vecvec())
  v_grp <- vec_group_loc(x$v)
  na_vec <- vapply(v_grp$key, is.null, logical(1L))
  i_loc <- cumsum(!na_vec)
  i_loc[na_vec] <- NA_integer_

  v <- .mapply(
    function(key, loc) vec_slice(key, unique(x$x[loc])),
    vec_slice(v_grp, !na_vec), NULL
  )
  ix_order <- order(list_unchop(v_grp$loc))
  return(
    vctrs::new_rcrd(
      list(
        i = rep(i_loc, lengths(v_grp$loc))[ix_order],
        x = list_unchop(lapply(lengths(v_grp$loc), seq_len))[ix_order]
      ),
      v = v,
      class = "vecvec"
    )
  )

  # ptypes <- lapply(x$x, vctrs::vec_ptype)
  # loc <- vctrs::vec_group_loc(ptypes)
  # loc$key <- vec_seq_along(loc)
  # rcrd <- list_unchop(.mapply(vec_recycle_common, loc, NULL))
  # if(is.null(rcrd)) return(new_vecvec())

  g <- vec_group_loc(x[[1L]])
  g <- vec_slice(g, !is.na(g$key))

  # Update value attributes
  attr(to, "v") <- .mapply(
    function(key, loc) attr(to, "v")[[key]][unique(x[[2L]][loc])],
    g, NULL
  )

  # Update index values
  x_avail <- list_unchop(g$loc)
  x$i[x_avail] <- c(vec_group_id(x[[1L]][x_avail]))
  x$x[unlist(g[[2L]])] <- unlist(lapply(g[[2L]], function(i) vec_group_id(x[[2L]][i])))

  # Restore rcrd type
  NextMethod()
}

vec_cast_from_vecvec <- function(x, to, ...) {
  out <- lapply(attr(x, "v"), vec_cast, to = to, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], new_data_frame(x), NULL))
}

vec_cast_to_vecvec <- function(x, to, ...) {
  new_vecvec(x)
}

#' @export
vec_ptype2.vecvec <- function(x, y, ...) {
  return(new_vecvec())
  vec_ptype2.vecvec.vecvec(vec_cast(x, new_vecvec()), vec_cast(y, new_vecvec()))
}

#' @export
vec_ptype.vecvec <- function(x, ...) {
  return(new_vecvec())
  out <- new_data_frame(list(i = integer(), x = integer()))
  attributes(out) <- attributes(x)
  out
}

#' @export
vec_ptype2.vecvec.vecvec <- function(x, y, ...) {
  return(new_vecvec())
  # Combine attributes
  attr(x, "v") <- c(attr(x, "v"), attr(y, "v"))
  x
}


#' @export
vec_cast.vecvec.vecvec <- function(x, to, ...) {
  return(x)

  # Match attributes with combined attributes
  field(x, "i") <- vec_match(attr(x, "v"), attr(to, "v"))[field(x, "i")]
  # Apply unified attributes
  attributes(x) <- attributes(to)

  x
}
