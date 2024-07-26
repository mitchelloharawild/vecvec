#' @export
new_vecvec <- function(...) {
  vecs <- list(...)
  size <- lengths(vecs)
  new_rcrd(
    list(
      i = rep(seq_along(size), size),
      x = unlist(lapply(size, seq_len))
    ),
    v = vecs,
    class = "vecvec"
  )
}


#' @export
format.vecvec <- function(x, ...) {
  out <- lapply(attr(x, "v"), format, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], vec_proxy(x), NULL))
}

#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  g <- vec_group_loc(x[[1L]])

  # Update value attributes
  attr(to, "v") <- .mapply(
    function(key, loc) attr(to, "v")[[key]][unique(x[[2L]][loc])],
    g, NULL
  )

  # Update index values
  x$i <- c(vec_group_id(x[[1L]]))
  x$x[unlist(g[[2L]])] <- unlist(lapply(g[[2L]], function(i) vec_group_id(x[[2L]][i])))

  # Restore rcrd type
  NextMethod()
}

vec_cast_from_vecvec <- function(x, to, ...) {
  out <- lapply(attr(x, "v"), vec_cast, to = to, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], vec_proxy(x), NULL))
}

vec_cast_to_vecvec <- function(x, to, ...) {
  new_vecvec(x)
}

#' @export
vec_ptype2.vecvec <- function(x, y, ...) {
  new_vecvec()
}

#' @export
vec_ptype.vecvec <- function(x, ...) {
  out <- new_data_frame(list(i = integer(), x = integer()))
  attributes(out) <- attributes(x)
  out
}

#' @export
vec_ptype2.vecvec.vecvec <- function(x, y, ...) {
  # Combine attributes
  attr(x, "v") <- c(attr(x, "v"), attr(y, "v"))
  x
}


#' @export
vec_cast.vecvec.vecvec <- function(x, to, ...) {
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

  g <- vec_group_id(vec_proxy(y))

  # Alternative implementation of shortcut indexing (seemingly slower)
  # g <- vec_group_loc(vec_proxy(x))
  # vec_chop(y, indices = g$loc)
  # vapply(g$loc, vec_slice, integer(1L), 1L)

  # Check for equivalence with y for faster result (possibly risky if length is involved in operation?)
  y_unique <- lapply(vec_split(x, g)$val, vec_unique)
  if (all(lengths(y_unique) == 1L)) {
    # Match unique y values to type groups and vector indices
    i <- vec_unique_loc(g)
    g <- vec_slice(vec_proxy(y), i)

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
