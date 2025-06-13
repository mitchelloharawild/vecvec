#' @export
Ops.vecvec <- function(e1, e2) {
  bool_op <- switch(
    .Generic,
    `<` = , `>` = , `==` = , `!=` = , `<=` = , `>=` = TRUE,
    FALSE
  )

  # Unary operation (shortcut method on attributes)
  if (missing(e2)) {
    attr(e1, "v") <- lapply(attr(e1, "v"), .Generic)
    return(e1)
  }

  args <- vec_cast_common(e1 = e1, e2 = e2)

  # Binary operation (shortcut method on attributes)
  if (any(arg_len1 <- lengths(args) == 1)) {
    res <- args[[which(!arg_len1)]]
    attr(res, "v") <- .mapply(.Generic, list(attr(res, "v")), attr(args[[which(arg_len1)]], "v"))
    return(if (bool_op) as.logical(res) else res)
  }

  # Binary operation (complete method on values)
  args <- vec_recycle_common(!!!args)

  # Compare sets of common vectors
  loc <- vec_group_loc(new_data_frame(lapply(args, field, "i")))
  loc[names(loc$key)] <- loc$key
  loc$key <- NULL

  # Apply operation to pairs of data types
  res <- .mapply(function(loc, e1, e2) {
    do.call(
      .Generic,
      list(
        attr(args$e1, "v")[[e1]][field(args$e1, "x")[loc]],
        attr(args$e2, "v")[[e2]][field(args$e2, "x")[loc]]
      )
    )
  }, loc, NULL)

  # Combine results into vector
  if(bool_op) {
    # Return atomic type for logical operations
    list_unchop(res)[order(list_unchop(loc$loc))]
  } else {
    # Return vecvec type for arith
    new_rcrd(
      vec_slice(
        data_frame(
          i = rep(seq_along(res), lengths(res)),
          x = list_unchop(lapply(lengths(res), seq_len))
        ),
        order(list_unchop(loc$loc))
      ),
      v = res,
      class = "vecvec"
    )
  }
}

#' @export
Math.vecvec <- function(x, ...) {
  attr(x, "v") <- lapply(attr(x, "v"), .Generic, ...)
  # Detect if all listed prototypes are compatible, then collapse if flat
  collapse_vecvec(x)
}
