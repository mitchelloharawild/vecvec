vecvec_flatten_adj <- function(x) {
  if (is_vecvec(x)) {
    x@x <- vecvec_flatten_adj(x@x)
    return(x)
  }
  k <- length(x)
  if (k < 2L) return(x)

  # Vector prototypes
  ptypes <- lapply(x, vec_slice, 0L)

  # A slot can be merged into its predecessor when they share a ptype and
  # neither is ALTREP. Runs of mergeable slots are combined in a single
  # vec_c() each, rather than copying the growing slot one merge at a time.
  same <- vapply(seq_len(k - 1L), function(j) identical(ptypes[[j]], ptypes[[j + 1L]]), logical(1L))
  alt <- vapply(x, is_altrep, logical(1L))

  new_run <- c(TRUE, !same | alt[-1L] | alt[-k])
  run <- cumsum(new_run)
  if (run[[k]] == k) return(x)

  lapply(unname(split(seq_len(k), run)), function(idx) {
    if (length(idx) == 1L) x[[idx]] else vec_c(!!!x[idx])
  })
}
