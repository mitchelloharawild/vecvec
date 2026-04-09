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
  # Convert to vecvec before recycling so vctrs repeats @i rather than
  # expanding the underlying vector (which would defeat compression).
  if (!is_vecvec(e1)) e1 <- vecvec(e1)
  if (!is_vecvec(e2)) e2 <- vecvec(e2)
  args <- vec_recycle_common(e1 = e1, e2 = e2)
  n <- vec_size(args[[1L]])

  # Cumulative slot boundaries: map @i -> (slot index, within-slot position).
  b <- lapply(args, function(a) c(0L, cumsum(lengths(a@x))))
  slot <- lapply(
    seq_along(args),
    function(i) findInterval(args[[i]]@i, b[[i]], left.open = TRUE)
  )
  within <- .mapply(
    function(arg, b, slot) arg@i - b[slot],
    list(args, b, slot), NULL
  )

  # Encode the (slot$e1, slot$e2) pair as a single key; contiguous runs define output slots.
  slot_pair_key <- (slot[[1L]] - 1L) * (length(args$e2@x) + 1L) + slot[[2L]]
  out_slot <- cumsum(c(TRUE, slot_pair_key[-1L] != slot_pair_key[-n]))
  n_slots <- out_slot[length(out_slot)]

  # For each output slot: compute the op on unique (w$e1, w$e2) pairs and record val_idx.
  # Returns list(vals, val_idx) so result_i can be built without a second pass.
  groups <- split(seq_len(n), out_slot)
  computed <- lapply(groups, function(pos) {
    w <- lapply(within, `[`, pos)
    s_idx <- lapply(slot, `[[`, pos[1L])

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

  result_x <- unname(lapply(computed, `[[`, "vals"))

  # Precompute per-slot offsets once, then scatter val_idx into result_i.
  offsets <- c(0L, cumsum(lengths(result_x))[-n_slots])
  result_i <- integer(n)
  for (s in seq_len(n_slots)) {
    result_i[groups[[s]]] <- offsets[[s]] + computed[[s]]$val_idx
  }

  res <- class_vecvec(result_x, result_i)

  if (bool_op) {
    res <- unvecvec(res)
  }
  res
}

#' @method Math vecvec::vecvec
#' @export
`Math.vecvec::vecvec` <- function(x, ...) {
  if (.Generic %in% c("cumsum, cumprod, cummax, cummin")) {
    rlang::abort("Culumative operations are not yet supported")
  }
  x@x <- lapply(x@x, .Generic, ...)
  # TODO - Detect if all listed prototypes are compatible, then collapse if flat
  x
}
