# S7-style implementation pending https://github.com/RConsortium/S7/issues/353
# Indicative release in S7 v0.3.0
#' @method Ops vecvec::vecvec
#' @export
`Ops.vecvec::vecvec` <- function(e1, e2) {
  bool_op <- switch(
    .Generic,
    `<` = , `>` = , `==` = , `!=` = , `<=` = , `>=` = TRUE,
    FALSE
  )

  # Unary operation (shortcut method on attributes)
  if (missing(e2)) {
    e1@x <- lapply(e1@x, .Generic)
    return(e1)
  }

  # Binary operation (shortcut method on attributes)
  # if (any(arg_len1 <- lengths(args) == 1)) {
  #   res <- args[[which(!arg_len1)]]
  #   attr(res, "v") <- .mapply(.Generic, list(attr(res, "v")), attr(args[[which(arg_len1)]], "v"))
  #
  #   # Return simple atomic boolean for boolean operations
  #   return(if (bool_op) unvecvec(res) else res)
  # }

  # Binary operation (complete method on values)
  args <- vec_recycle_common(e1 = e1, e2 = e2)

  # Ensure all args are vecvec types
  which_vecvec <- vapply(args, is_vecvec, logical(1L))
  args[!which_vecvec] <- lapply(args[!which_vecvec], vecvec)

  # Find pairing of underlying vectors
  x_len <- lapply(args, function(x) lengths(x@x))
  x_idx <- overlap_indices(x_len[[1L]], x_len[[2L]])

  split_vec <- function(x, len, idx) {
    vec <- vector("list", length(len))
    j <- 1L
    for (i in seq_along(len)) {
      vec[[i]] <- x@x[[idx[i]]][seq(j, length.out = len[i])]
      j <- if (identical(idx[i], idx[i+1L])) j + len[i] else 1L
    }
    vec
  }

  # Apply operation to pairs of data types
  e1@x <- .mapply(
    .Generic,
    list(
      split_vec(args[[1L]], x_idx$len, x_idx$idx[[1L]]),
      split_vec(args[[2L]], x_idx$len, x_idx$idx[[2L]])
    ), 
    NULL
  )

  # Combine results into vector
  if(bool_op) {
    # Return atomic type for logical operations
    e1 <- unvecvec(e1)
  }
  
  e1
}

#' @method Math vecvec::vecvec
#' @export
`Math.vecvec::vecvec` <- function(x, ...) {
  if(.Generic %in% c("cumsum, cumprod, cummax, cummin")) {
    rlang::abort("Culumative operations are not yet supported")
  }
  x@x <- lapply(x@x, .Generic, ...)
  # TODO - Detect if all listed prototypes are compatible, then collapse if flat
  x
}
