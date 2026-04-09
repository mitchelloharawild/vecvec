#' @export
`duplicated.vecvec::vecvec` <- function(x, incomparables = FALSE, ...) {
  # Special case for empty vecvec vectors
  if (length(x@x) == 0L) return(duplicated(x@i, incomparables, ...))

  # Find common vector types
  ptypes <- lapply(x@x, `[`, 0L)

  # Analagous to vctrs::vec_group_loc
  key <- unique(ptypes)
  loc  <- lapply(key, function(k) which(vapply(ptypes, identical, logical(1), k)))

  # Identify duplicated values within common vector types
  dup <- lapply(loc, function(i) {
    # Compute duplicates on a single vector
    vec <- unlist(x@x[i], recursive = FALSE)
    res <- duplicated(vec, incomparables = incomparables, ...)

    # Restructure result into list of vectors
    idx <- c(0L, cumsum(lengths(x@x[i])))
    len <- length(i)
    out <- vector("list", len)
    for (i in seq_len(len)) {
      out[[i]] <- res[seq(idx[i] + 1L, idx[i + 1])]
    }
    out
  })

  x@x[unlist(loc, recursive = FALSE)] <- unlist(dup, recursive = FALSE)
  unvecvec(x, ptype = logical())
}
