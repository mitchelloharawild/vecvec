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
  x$i <- vctrs::vec_group_id(x[[1L]])
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
