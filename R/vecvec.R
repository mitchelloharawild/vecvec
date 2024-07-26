#' @export
new_vecvec <- function(...) {
  vecs <- list(...)
  size <- lengths(vecs)
  vctrs::new_rcrd(
    list(
      i = rep(seq_along(size), size),
      x = unlist(lapply(size, seq_len))
    ),
    v = vecs,
    class = "vecvec"
  )
}


#' @export
#' @importFrom vctrs vec_proxy
format.vecvec <- function(x, ...) {
  out <- lapply(attr(x, "v"), format, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], vec_proxy(x), NULL))
}

#' @importFrom vctrs vec_restore vec_group_loc
#' @export
vec_restore.vecvec <- function(x, to, ..., i = NULL) {
  g <- vec_group_loc(x[[1L]])

  # Update value attributes
  attr(to, "v") <- .mapply(
    function(key, loc) attr(to, "v")[[key]][unique(x[[2L]][loc])],
    g, NULL
  )

  # Update index values
  x$i <- c(vctrs::vec_group_id(x[[1L]]))
  x$x[unlist(g[[2L]])] <- unlist(lapply(g[[2L]], function(i) vctrs::vec_group_id(x[[2L]][i])))

  # Restore rcrd type
  NextMethod()
}

#' @importFrom vctrs vec_cast vec_proxy
vec_cast_from_vecvec <- function(x, to, ...) {
  out <- lapply(attr(x, "v"), vec_cast, to = to, ...)
  unlist(.mapply(function(i, x) out[[i]][[x]], vec_proxy(x), NULL))
}

#' @importFrom vctrs vec_cast
vec_cast_to_vecvec <- function(x, to, ...) {
  new_vecvec(x)
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.vecvec <- function(x, y, ...) {
  new_vecvec()
}

#' @importFrom vctrs vec_ptype
#' @export
vec_ptype.vecvec <- function(x, ...) {
  out <- vctrs::new_data_frame(list(i = integer(), x = integer()))
  attributes(out) <- attributes(x)
  out
}

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.vecvec.vecvec <- function(x, y, ...) {
  # Combine attributes
  attr(x, "v") <- c(attr(x, "v"), attr(y, "v"))
  x
}


#' @importFrom vctrs vec_cast field field<-
#' @export
vec_cast.vecvec.vecvec <- function(x, to, ...) {
  # Match attributes with combined attributes
  field(x, "i") <- vctrs::vec_match(attr(x, "v"), attr(to, "v"))[field(x, "i")]
  # Apply unified attributes
  attributes(x) <- attributes(to)

  x
}

#' @importFrom vctrs vec_math
#' @export
vec_math.vecvec <- function(.fn, .x, ...) {
  attr(.x, "v") <- lapply(attr(.x, "v"), .Generic, .fn = .fn, ...)
  # Detect if all listed prototypes are compatible, then collapse if flat
  collapse_vecvec(.x)
}

#' @importFrom vctrs vec_arith
#' @method vec_arith vecvec
#' @export
vec_arith.vecvec <- function(op, x, y, ...) {
  UseMethod("vec_arith.vecvec", y)
}

#' @importFrom vctrs vec_arith
#' @method vec_arith.vecvec vecvec
#' @export
vec_arith.vecvec.vecvec <- function(op, x, y, ...) {
  stop("Cannot yet perform arithmetic on two vecvecs")
}

#' @importFrom vctrs vec_arith vec_recycle vec_size
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
