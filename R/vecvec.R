new_vecvec <- function(...) {
  vecs <- list(...)
  size <- lengths(vecs)
  vctrs::new_rcrd(
    list(
      i = rep(seq_along(size), size),
      x = unlist(lapply(size, seq_len))
    ),
    vecs = vecs,
    class = "vecvec"
  )
}


#' @export
#' @importFrom vctrs field
format.vecvec <- function(x, ...) {
  out <- lapply(attr(x, "vecs"), format)
  unlist(mapply(function(i, x) out[[i]][[x]], field(x, "i"), field(x, "x")))
}
