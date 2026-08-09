method(unique, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  x[!duplicated(x)]
}

method(diff, class_vecvec) <- function (x, lag = 1L, differences = 1L, ...)
{
  ismat <- is.matrix(x)
  if (length(lag) != 1L || length(differences) != 1L || lag < 1L || differences < 1L) {
    cli::cli_abort("{.arg lag} and {.arg differences} must be integers >= 1.")
  }
  i1 <- -seq_len(lag)
  if (ismat) for (i in seq_len(differences))
    x <- x[i1, , drop = FALSE] - x[seq_len(max(nrow(x) - lag, 0L)), , drop = FALSE]
  else for (i in seq_len(differences))
    x <- x[i1] - `length<-`(x, max(length(x) - lag, 0L))
  x
}

#' @export
method(all.equal, class_vecvec) <- function(target, current, ...) {
  if (!is_vecvec(current)) {
    current <- vecvec(current)
  }

  lt <- length(target)
  lc <- length(current)
  if (lt != lc) {
    return(paste0("Lengths (", lt, ", ", lc, ") differ"))
  }
  if (lt == 0L) {
    return(TRUE)
  }

  idx_t <- S7_data(target)
  idx_c <- S7_data(current)
  na_t <- is.na(idx_t)
  na_c <- is.na(idx_c)

  msg <- character()

  if ((n_na <- sum(na_t != na_c)) > 0L) {
    msg <- c(msg, sprintf("%d out of %d elements are missing in only one object", n_na, lt))
  }

  pos <- which(!na_t & !na_c)
  if (length(pos) == 0L) {
    return(if (length(msg)) msg else TRUE)
  }

  # Map each compared position to its (slot, within-slot) location in both
  # objects, then group by the pair of slots involved - elements sharing a
  # slot pair are compared together (vectorised), rather than one at a time.
  at_t <- vecvec_locate(target@x, idx_t[pos])
  at_c <- vecvec_locate(current@x, idx_c[pos])

  groups <- split(seq_along(pos), vecvec_pair_key(at_t$slot, at_c$slot, length(current@x)))
  for (g in groups) {
    vt <- vec_slice(target@x[[at_t$slot[g[1L]]]], at_t$within[g])
    vc <- vec_slice(current@x[[at_c$slot[g[1L]]]], at_c$within[g])
    cmp <- all.equal(vt, vc, ...)
    if (!isTRUE(cmp)) {
      msg <- c(msg, cmp)
    }
  }

  if (length(msg)) msg else TRUE
}