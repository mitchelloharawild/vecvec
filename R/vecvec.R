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
  if(identical(y, vctrs::MISSING())) {
    attr(x, "v") <- .mapply(op, list(x = attr(x, "v")), NULL)
    return(x)
  }

  y <- vec_recycle(y, vec_size(x))

  # Apply arithmetic to each vector in the vecvec
  g <- lengths(attr(x, "v"), y)
  g <- rep.int(seq_along(g), g)

  attr(x, "v") <- .mapply(vec_arith, list(x = attr(x, "v"), y = split(y, g)), list(op = op, ...))
  x
}

vec_default_arith_vecvec <- function(op, x, y, ...) {
  x <- vec_recycle(x, vec_size(y))

  # Apply arithmetic to each vector in the vecvec
  g <- lengths(attr(y, "v"), x)
  g <- rep.int(seq_along(g), g)

  attr(y, "v") <- .mapply(vec_arith, list(x = split(x, g), y = attr(y, "v")), list(op = op, ...))
  y
}
