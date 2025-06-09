#' @export
new_vecvec <- function(...) {
  vecs <- list(...)
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
  if (is.null(ptype)) ptype <- do.call(vec_ptype_common, attr(x, "v"))
  attr(x, "v") <- lapply(attr(x, "v"), vec_cast, to = ptype)

  out <- .mapply(function(key, val) attr(x, "v")[[key]][val], vec_split(field(x, "x"), field(x, "i")), NULL)
  unsplit(out, field(x, "i"))
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
  return(out)
  new_data_frame(list(x = out))
  # structure(new_data_frame(x), v = attr(x, "v"))
}

#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  # TODO: combine common groups
  if(vec_is_empty(x)) return(new_vecvec())
  v <- vec_group_loc(x$v)
  na_vec <- vapply(v$key, is.null, logical(1L))
  i_loc <- cumsum(!na_vec)
  i_loc[na_vec] <- NA_integer_
  x[["i"]][list_unchop(v$loc)] <- rep(i_loc, lengths(v$loc))
  v <- .mapply(
    function(key, loc) vec_slice(key, unique(x$x[loc])),
    vec_slice(v, !na_vec), NULL
  )
  return(
    vctrs::new_rcrd(x[c("i", "x")], v = v, class = "vecvec")
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

#' @export
vec_math.vecvec <- function(.fn, .x, ...) {
  attr(.x, "v") <- lapply(attr(.x, "v"), .Generic, .fn = .fn, ...)
  # Detect if all listed prototypes are compatible, then collapse if flat
  collapse_vecvec(.x)
}

#' @method vec_arith vecvec
#' @export
vec_arith.vecvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.vecvec", y)
}

#' @method vec_arith.vecvec vecvec
#' @export
vec_arith.vecvec.vecvec <- function(op, x, y, ...) {
  stop("Cannot yet perform arithmetic on two vecvecs")
}

#' @method vec_arith.vecvec default
#' @export
vec_arith.vecvec.default <- function(op, x, y, ...) {
  # For !, unary + and unary -
  if(identical(y, MISSING())) {
    attr(x, "v") <- .mapply(op, list(x = attr(x, "v")), NULL)
    return(x)
  }

  vec_default_arith_vecvec(op, x = y, y = x, ..., .xyflip = TRUE)
}

vec_default_arith_vecvec <- function(op, x, y, ..., .xyflip = FALSE) {
  x <- vec_recycle(x, vec_size(y))

  g <- vec_group_id(new_data_frame(y))

  # Alternative implementation of shortcut indexing (seemingly slower)
  # g <- vec_group_loc(new_data_frame(x))
  # vec_chop(y, indices = g$loc)
  # vapply(g$loc, vec_slice, integer(1L), 1L)

  # Check for equivalence with y for faster result (possibly risky if length is involved in operation?)
  y_unique <- lapply(vec_split(x, g)$val, vec_unique)
  if (all(lengths(y_unique) == 1L)) {
    # Match unique y values to type groups and vector indices
    i <- vec_unique_loc(g)
    g <- vec_slice(new_data_frame(y), i)

    # If indices are strictly ordered within groups
    x <- vec_split(vec_slice(x, i), g$i)$val

    # If order matching is needed
    # g$y <- vec_slice(y, i)
    # g <- vec_split(g[-1L], g$i)$val
    # y <- lapply(g, function(x) x[[2L]][x[[1L]]])
  } else {
    # Expand the vecvec to accommodate the new calculations
    attr(y, "v") <- .mapply(function(key, val) attr(y, "v")[[key]][val], vec_split(field(y, "x"), field(y, "i")), NULL)
    g <- vec_group_loc(field(y, "i"))
    field(y, "x")[list_unchop(g$loc)] <- list_unchop(lapply(g$loc, seq_along))
    x <- vec_chop(x, indices = g$loc)
  }

  # Apply calculation
  xyargs <- list(x = x, y = attr(y, "v"))
  if(.xyflip) names(xyargs) <- c("y", "x")
  attr(y, "v") <- .mapply(vec_arith, xyargs, list(op = op, ...))
  y
}
