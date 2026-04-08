#' Compute overlap segments between two integer run-length vectors
#'
#' @description
#' Given two integer vectors `a` and `b` representing run lengths
#' along a common 1D axis (e.g. segments of contiguous positions),
#' this function computes the lengths of consecutive overlapping segments
#' between them. It assumes that `a` and `b` are positive integers and that 
#' their sums are equal (i.e. lengths along same-sized vecvecs)
#'
#' Conceptually, you"“walk" along the sums of `a` and `b` simultaneously,
#' taking at each step the minimum of the remaining length in the current
#' element of `a` and `b`, and recording that as one overlap segment.
#'
#' @param a Integer vector of positive run lengths.
#' @param b Integer vector of positive run lengths.
#'
#' @return
#' An integer vector giving the lengths of consecutive overlapping segments.
#'
#' @details
#' This is an \eqn{O(length(a) + length(b))} algorithm using a single pass
#' through both vectors with constant additional memory, apart from the
#' preallocated output.
#'
#' @examples
#' a <- c(4L, 8L, 3L)
#' b <- c(3L, 9L, 3L)
#'
#' overlap_indices(a, b)
#' # [1] 3 1 8 3
#'
#' @noRd
overlap_indices <- function(a, b) {
  n_a <- length(a)
  n_b <- length(b)
  
  # Upper bound (very loose but safe)
  len <- i_a <- i_b <- integer(n_a + n_b - 1L)
  
  k <- 0L
  
  i <- 1L
  j <- 1L
  ra <- a[i]
  rb <- b[j]
  
  while (i <= n_a && j <= n_b) {
    x <- if (ra < rb) ra else rb
    k <- k + 1L
    len[k] <- x
    i_a[k] <- i
    i_b[k] <- j
    
    ra <- ra - x
    rb <- rb - x
    
    if (ra == 0L) {
      i <- i + 1L
      if (i <= n_a) ra <- a[i]
    }
    if (rb == 0L) {
      j <- j + 1L
      if (j <= n_b) rb <- b[j]
    }
  }
  
  # Trim to actual used length
  list(
    len = len[seq_len(k)],
    idx = list(
      i_a = i_a[seq_len(k)],
      i_b = i_b[seq_len(k)]
    )
  )
}