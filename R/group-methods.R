# S7-style implementation pending https://github.com/RConsortium/S7/issues/353
# Indicative release in S7 v0.3.0
#' @method Ops vecvec::vecvec
#' @export
`Ops.vecvec::vecvec` <- function(e1, e2) {
  bool_op <- switch(
    .Generic,
    `<` = ,
    `>` = ,
    `==` = ,
    `!=` = ,
    `<=` = ,
    `>=` = TRUE,
    FALSE
  )

  # Unary operation (shortcut method on attributes)
  if (missing(e2)) {
    e1@x <- lapply(e1@x, .Generic)
    return(e1)
  }

  # Binary operation (complete method on values)
  # Convert to vecvec before recycling so vctrs repeats indices rather than
  # expanding the underlying vector (which would defeat compression).
  if (!is_vecvec(e1)) e1 <- vecvec(e1) else class_vv <- S7_class(e1)
  if (!is_vecvec(e2)) e2 <- vecvec(e2) else class_vv <- S7_class(e2)
  args <- vec_recycle_common(e1 = e1, e2 = e2)
  n <- vec_size(args[[1L]])

  # Map indices -> (slot index, within-slot position), and group positions
  # into contiguous runs drawing from the same pair of underlying slots.
  al <- vecvec_align(args)
  slot <- al$slot
  within <- al$within
  groups <- al$groups
  n_slots <- length(groups)

  # For each output slot: compute the op on unique (w$e1, w$e2) pairs and record val_idx.
  # Returns list(vals, val_idx) so result_i can be built without a second pass.
  # A group whose slot is NA (e1 or e2 is missing at that position) isn't
  # backed by a real value in either operand's slots, so it's left out of
  # result_x entirely and mapped to a missing (NA) index in result_i below,
  # rather than fabricating a value to run .Generic on.
  computed <- lapply(groups, function(pos) {
    s_idx <- lapply(slot, `[[`, pos[1L])
    if (anyNA(s_idx)) {
      return(list(vals = NULL, val_idx = integer(0L)))
    }

    w <- lapply(within, `[`, pos)
    pair_key <- (w[[1L]] - 1L) * length(args[[2L]]@x[[s_idx[[2L]]]]) + w[[2L]]
    unique_keys <- unique(pair_key)
    first_of <- match(unique_keys, pair_key)

    list(
      vals = do.call(
        .Generic,
        .mapply(
          function(arg, s, w) arg@x[[s]][w][first_of],
          list(args, s_idx, w), NULL
        )
      ),
      val_idx = match(pair_key, unique_keys)
    )
  })

  is_na_group <- vapply(computed, function(cs) is.null(cs$vals), logical(1L))
  result_x <- unname(lapply(computed[!is_na_group], `[[`, "vals"))

  # Precompute per-slot offsets once, then scatter val_idx into result_i.
  offsets <- if (length(result_x)) c(0L, cumsum(lengths(result_x))[-length(result_x)]) else integer(0L)
  result_i <- rep(NA_integer_, n)
  vi <- 0L
  for (s in seq_len(n_slots)) {
    if (is_na_group[[s]]) next
    vi <- vi + 1L
    result_i[groups[[s]]] <- offsets[[vi]] + computed[[s]]$val_idx
  }

  # TODO - use a method to identify if a better class can be returned.
  res <- class_vv(result_x, result_i)

  if (bool_op) {
    res <- unvecvec(res)
  }
  res
}

#' @method Math vecvec::vecvec
#' @export
`Math.vecvec::vecvec` <- function(x, ...) {
  if (.Generic %in% c("cumsum", "cumprod", "cummax", "cummin")) {
    generic <- .Generic
    cli::cli_abort(
      c(
        "{.fn {generic}} is not supported for {.cls vecvec} objects.",
        "i" = "Call {.fn unvecvec} first if the slots share a common type."
      )
    )
  }
  x@x <- lapply(x@x, .Generic, ...)
  # TODO - Detect if all listed prototypes are compatible, then collapse if flat
  x
}

#' @method Summary vecvec::vecvec
#' @export
`Summary.vecvec::vecvec` <- function(..., na.rm = FALSE) {
  do.call(
    .Generic,
    c(
      lapply(
        rlang::list2(...),
        function(x) {if (is_vecvec(x)) unvecvec(x) else x}
      ),
      list(na.rm = na.rm)
    )
  )
}

#' @method Complex vecvec::vecvec
#' @export
`Complex.vecvec::vecvec` <- function(z) {
  z@x <- lapply(z@x, .Generic)
  unvecvec(z)
}