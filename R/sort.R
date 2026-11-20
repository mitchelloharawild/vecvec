# ------------------------------------------------------------------------------
# Ordering (sort(), order(), rank(), vctrs::vec_order(), ...)
# ------------------------------------------------------------------------------
#
# A single-type vecvec sorts as its type. A mixed-type vecvec whose types have
# a common type (e.g. integers and doubles) sorts by its values as that type.
# Otherwise its values are sorted by type, with types in the order they first
# appear, and then by value within each type. Missing values sort last.

method(xtfrm, class_vecvec) <- function(x) {
  if (vv_is_typed(x)) return(xtfrm(vv_typed_slot(x)))
  slots <- vv_slots(x)
  if (!is.null(vv_common_ptype(slots))) return(xtfrm(unvecvec(x)))

  # A key from the position of each type and the rank of each value within it
  at <- vecvec_locate(slots, vv_index(x))
  types <- unique(at$slot[!is.na(at$slot)])
  m <- max(list_sizes(slots))
  key <- rep(NA_real_, length(at$slot))
  for (t in seq_along(types)) {
    s <- types[[t]]
    r <- which(at$slot == s)
    rank_in_type <- rank(xtfrm(slots[[s]]), ties.method = "min", na.last = "keep")
    key[r] <- (t - 1) * (m + 1) + rank_in_type[at$within[r]]
  }
  key
}

# The comparison proxy of a mixed-type vecvec. vctrs compares the proxies of
# two vecvecs column by column (after casting them to their common type), so
# the layout is fixed: the position of each element's type (in order of first
# appearance, or 1 for all when the types have a common type), and its value
# in `num` or `chr`. Values of types whose comparison proxy isn't a number or a
# string are ranked within the vecvec. Missing elements are missing in every
# column.
vv_proxy_compare_mixed <- function(x, ...) {
  n <- vec_size(x)
  type <- rep_len(NA_integer_, n)
  num <- rep_len(0, n)
  chr <- rep_len("", n)
  slots <- vv_slots(x)

  common <- vv_common_ptype(slots)
  if (!is.null(common)) {
    # One "type" of the common type
    slots <- list(unvecvec(x))
    at <- list(slot = rep_len(1L, n), within = seq_len(n))
  } else {
    at <- vecvec_locate(slots, vv_index(x))
  }
  types <- unique(at$slot[!is.na(at$slot)])

  for (t in seq_along(types)) {
    s <- types[[t]]
    r <- which(at$slot == s)
    p <- vec_slice(vec_proxy_compare(slots[[s]], ...), at$within[r])
    if (!is.data.frame(p) && typeof(p) %in% c("logical", "integer", "double", "character")) {
      v <- unclass(p)
      attributes(v) <- NULL
      if (is.character(v)) chr[r] <- v else num[r] <- as.double(v)
    } else {
      num[r] <- vec_rank(p, ties = "min", incomplete = "na")
    }
    type[r] <- t
    miss <- r[vec_detect_missing(p)]
    type[miss] <- NA_integer_
    num[miss] <- NA_real_
    chr[miss] <- NA_character_
  }
  missing_index <- which(is.na(at$slot))
  num[missing_index] <- NA_real_
  chr[missing_index] <- NA_character_

  new_data_frame(list(type = type, num = num, chr = chr), n = n)
}

# The common type of `slots`, or NULL if they have none.
vv_common_ptype <- function(slots) {
  if (length(slots) == 0L) return(NULL)
  tryCatch(
    vec_ptype_common(!!!slots),
    vctrs_error_incompatible_type = function(e) NULL
  )
}
