method(unique, class_vecvec) <- function(x, incomparables = FALSE, ...) {
  # x@i <- unique(x@i)
  x[!duplicated(x)]
}

method(diff, class_vecvec) <- function (x, lag = 1L, differences = 1L, ...) 
{
  ismat <- is.matrix(x)
  if (length(lag) != 1L || length(differences) != 1L || lag < 1L || differences < 1L)
  	stop("'lag' and 'differences' must be integers >= 1")
  i1 <- -seq_len(lag)
  if (ismat) for (i in seq_len(differences))
    x <- x[i1, , drop = FALSE] - x[seq_len(max(nrow(x) - lag, 0L)), , drop = FALSE] 
  else for (i in seq_len(differences))
    x <- x[i1] - `length<-`(x, max(length(x) - lag, 0L))
  x
}