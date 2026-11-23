# ------------------------------------------------------------------------------
# How a vecvec is stored
# ------------------------------------------------------------------------------
#
# A vecvec is an ordinary R vector with attributes, in one of two forms:
#
# * single-type ("typed"): all elements share one type (a single slot) and that
#   type is held in an atomic vector. The data of the vecvec *is* that vector's
#   data (without its attributes), and the vector's attributes are kept in a
#   zero-length prototype, the `vecvec_ptype` attribute. The slot is restored by
#   putting those attributes back on the data.
#
# * mixed-type ("indexed"): anything else (several slots, or a slot that isn't
#   atomic, like a record or a data frame). The data of the vecvec is an integer
#   index into the concatenation of the slots, which are kept as a list in the
#   `vecvec_x` attribute.
#
# Both forms keep `dim`, `dimnames` and `names` on the data, as base R does.
#
# Everything a vecvec needs is in its data and its attributes, so C code that
# copies a vector's data and then its attributes (data.table, tidyr, reshape2,
# serialization, ...) produces a valid vecvec. C code that reads the data
# directly (lm(), model.matrix(), ...) sees the values of a single-type vecvec,
# and the index of a mixed-type one, which R/model.R stops it from using.
#
# The form is chosen by vv_new_data() whenever a vecvec is created, so the
# representation of a vecvec is canonical: slots never share a type (identical
# prototypes), and a vecvec is single-type whenever it can be.

vv_layout_attrs <- c("dim", "dimnames", "names")
vv_ptype_attr <- "vecvec_ptype"
vv_slots_attr <- "vecvec_x"
vv_mixed_attr <- "vecvec_mixed"

# Is `x` stored in the single-type form?
vv_is_typed <- function(x) !is.null(attr(x, vv_ptype_attr, exact = TRUE))

# Is `x` a single-type vecvec to vctrs (see R/vctrs.R)? A single-type vecvec
# cast to a mixed-type vecvec keeps its values, with the `vecvec_mixed`
# attribute marking it as a vector of any types until it is next rebuilt.
vv_is_single <- function(x) {
  vv_is_typed(x) && is.null(attr(x, vv_mixed_attr, exact = TRUE))
}

# The data of a vecvec: its values (single-type) or index (mixed-type), with
# only the layout attributes (dim, dimnames and names). Changing attributes
# doesn't copy the data of long vectors (R wraps them), so this is cheap.
vv_data <- function(x) {
  at <- attributes(x)
  attributes(x) <- at[names(at) %in% vv_layout_attrs]
  x
}

# Replace the data of a single-type vecvec with `data`, a vector of the same
# type, keeping its dim, dimnames and names (not those of `x`).
vv_with_data <- function(x, data) {
  at <- attributes(x)
  at[[vv_mixed_attr]] <- NULL
  ad <- attributes(data)
  attributes(data) <- c(
    ad[names(ad) %in% vv_layout_attrs],
    at[!names(at) %in% vv_layout_attrs]
  )
  data
}

# The integer index of a vecvec into the concatenation of its slots, with its
# dim, dimnames and names.
vv_index <- function(x) {
  d <- vv_data(x)
  if (vv_is_typed(x)) {
    i <- seq_len(length(d))
    attributes(i) <- attributes(d)
    return(i)
  }
  if (!is.integer(d)) {
    cli::cli_abort(
      c(
        "Can't read a mixed-type vecvec whose index isn't an integer vector.",
        "i" = "Its data was replaced by code that doesn't know about vecvecs."
      ),
      call = NULL
    )
  }
  d
}

# The stored vectors of a vecvec (the value of `@x`).
vv_slots <- function(x) {
  p <- attr(x, vv_ptype_attr, exact = TRUE)
  if (!is.null(p)) {
    return(list(vv_typed_slot(x, p)))
  }
  slots <- attr(x, vv_slots_attr, exact = TRUE) %||% vv_legacy_slots(x)
  if (is.null(slots)) {
    cli::cli_abort(
      c(
        "Can't find the stored vectors of a vecvec.",
        "i" = "It was copied without its attributes."
      ),
      call = NULL
    )
  }
  slots
}

# vecvec <= 1.4.0 stored an integer index with the slots in an `x` attribute
# (e.g. in objects saved with saveRDS()). They are read as mixed-type vecvecs,
# and anything made from them has the current layout (see vv_rebuild()).
vv_legacy_slots <- function(x) {
  if (is.integer(x)) attr(x, "x", exact = TRUE)
}

# The single slot of a single-type vecvec: its data with the attributes of the
# prototype.
vv_typed_slot <- function(x, ptype = attr(x, vv_ptype_attr, exact = TRUE)) {
  attributes(x) <- attributes(ptype)
  x
}

# The prototype of a single-type vecvec.
vv_typed_ptype <- function(x) attr(x, vv_ptype_attr, exact = TRUE)

# Can a slot be held in the single-type form? Its data must be an atomic vector
# with one value per element (so not a matrix, or a list-based type such as a
# record, data frame or POSIXlt), with attributes that don't depend on its
# length (as vctrs assumes when restoring a slice of a classed vector). Names
# are dropped, and time series are kept out (their `tsp` depends on length).
vv_can_type <- function(slot) {
  is.atomic(slot) && !isS4(slot) &&
    is.null(attr(slot, "dim", exact = TRUE)) &&
    is.null(attr(slot, "tsp", exact = TRUE)) &&
    vec_size(slot) == length(unclass(slot))
}

# Zero-length prototype of an atomic slot, keeping all its attributes but names.
vv_atomic_ptype <- function(slot) {
  at <- attributes(slot)
  at[["names"]] <- NULL
  p <- vector(typeof(slot), 0L)
  attributes(p) <- at
  p
}

# The key slots are grouped by: slots with identical keys have the same type
# and are combined.
vv_slot_key <- function(slot) {
  if (vv_can_type(slot)) vv_atomic_ptype(slot) else vec_ptype(slot)
}

# Is a slot only missing values of no particular type (a bare logical vector
# of NAs)?
vv_is_unspecified <- function(slot) {
  is.logical(slot) && is.null(attributes(slot)) && all(is.na(slot))
}

# Is `i` the sequence 1, ..., n? Compact sequences (1:n, seq_len(n)) know they
# are sorted, so this is O(1) for the default index.
vv_is_identity <- function(i, n) {
  m <- length(i)
  if (m != n) return(FALSE)
  if (m == 0L) return(TRUE)
  !anyNA(i[c(1L, m)]) && i[[1L]] == 1L && i[[m]] == n &&
    isFALSE(is.unsorted(i, na.rm = FALSE, strictly = TRUE))
}

# Are `x` and `y` (prototypes) the same type? The constructors of S7 classes in
# a prototype get new environments when it's deserialised (e.g. with
# readRDS()), so closure environments are ignored: the same type read back from
# a file is still the same type.
vv_same_type <- function(x, y) {
  identical(x, y) || identical(x, y, ignore.environment = TRUE)
}

# Group `keys` by type (see vv_same_type()), numbering groups in order of
# first appearance.
vv_group_identical <- function(keys) {
  k <- length(keys)
  grp <- integer(k)
  uniq <- list()
  for (s in seq_len(k)) {
    g <- 0L
    for (u in seq_along(uniq)) {
      if (vv_same_type(uniq[[u]], keys[[s]])) {
        g <- u
        break
      }
    }
    if (g == 0L) {
      uniq[[length(uniq) + 1L]] <- keys[[s]]
      g <- length(uniq)
    }
    grp[s] <- g
  }
  grp
}

# Canonicalise slots and an index: drop slots the index doesn't use, combine
# slots of the same type and remap the index to match. Indices outside the
# slots become NA. When the index uses no slot at all (it is empty or all NA),
# see vv_normalise_ptypes().
#
# @param slots An unnamed list of vectors.
# @param index A bare integer vector.
# @return A list with `slots` and `index`.
vv_normalise <- function(slots, index) {
  k <- length(slots)
  if (k == 0L) {
    return(list(slots = slots, index = rep_len(NA_integer_, length(index))))
  }
  sizes <- list_sizes(slots)

  if (k == 1L) {
    # A single slot (as when slicing a single-type vecvec): only indices
    # outside it need changing.
    flags <- .Call("vecvec_index_check", index, sizes, PACKAGE = "vecvec")
    if (bitwAnd(flags, 1L)) index[index < 1L | index > sizes] <- NA_integer_
    if (bitwAnd(flags, 2L)) {
      return(vv_normalise_ptypes(slots, index, vv_is_unspecified(slots[[1L]])))
    }
    return(list(slots = slots, index = index))
  }
  bounds <- c(0L, cumsum(sizes))

  # Missing values of no particular type (bare logical NAs, as vctrs treats
  # them) don't take a slot of their own next to values of other types: they
  # become missing indices, so e.g. c(x, NA) keeps a single-type x single-type.
  unspec <- vapply(slots, vv_is_unspecified, logical(1L))
  drop_unspec <- any(unspec) && !all(unspec)

  if (!drop_unspec && vv_is_identity(index, bounds[[k + 1L]])) {
    # Every stored value is used once, in order (as when constructing)
    slot_of <- NULL
    used <- sizes > 0L
  } else {
    slot_of <- .Call("vecvec_locate_c", index, sizes, PACKAGE = "vecvec")$slot
    if (drop_unspec) {
      slot_of[unspec[slot_of] %in% TRUE] <- NA_integer_
    }
    index[is.na(slot_of)] <- NA_integer_
    used <- tabulate(slot_of, nbins = k) > 0L
  }

  # Slots used by the index
  if (!any(used)) {
    return(vv_normalise_ptypes(slots, index, unspec))
  }
  keep <- which(used)

  # Group the kept slots by type, ordering groups by first appearance and
  # slots within a group by their position.
  grp <- vv_group_identical(lapply(slots[keep], vv_slot_key))
  if (length(keep) == k && !anyDuplicated(grp)) {
    return(list(slots = slots, index = index))
  }
  ord <- keep[order(grp)]

  # Shift each index by how far its slot moved
  if (is.null(slot_of)) slot_of <- rep.int(seq_len(k), sizes)
  new_start <- rep(NA_integer_, k)
  new_start[ord] <- c(0L, cumsum(sizes[ord]))[seq_along(ord)]
  shift <- new_start - bounds[-(k + 1L)]
  index <- index + shift[slot_of]

  members <- split(ord, sort(grp))
  slots <- lapply(unname(members), function(s) {
    if (length(s) == 1L) slots[[s]] else vec_c(!!!unname(slots[s]))
  })
  list(slots = slots, index = index)
}

# Drop the stored values that `index` doesn't use, keeping their order, and
# remap `index` to match. Slots left empty are dropped by vv_normalise().
#
# @param slots An unnamed list of vectors.
# @param index A bare integer vector of positions into the slots (or NA).
# @return A list with `slots` and `index`.
vv_compact <- function(slots, index) {
  sizes <- list_sizes(slots)
  used <- tabulate(index, nbins = sum(sizes)) > 0L
  if (all(used)) {
    return(list(slots = slots, index = index))
  }
  index <- cumsum(used)[index]
  ends <- cumsum(sizes)
  slots <- .mapply(
    function(s, from, to) {
      u <- used[seq.int(from, length.out = to - from + 1L)]
      if (all(u)) s else vec_slice(s, u)
    },
    list(slots, ends - sizes + 1L, ends), NULL
  )
  list(slots = slots, index = index)
}

# Canonicalise slots that no element uses (the index is empty or all missing).
# A vecvec of a single atomic type keeps its type (as a zero-length slot, which
# becomes the prototype of the single-type form). Otherwise no slots are kept:
# the slots of a mixed-type vecvec are exactly the values it uses, so code
# working on its slots never sees an empty one.
vv_normalise_ptypes <- function(slots, index, unspec) {
  if (!all(unspec)) slots <- slots[!unspec]
  keys <- lapply(slots, vv_slot_key)
  if (all(vv_group_identical(keys) == 1L) && vv_can_type(slots[[1L]])) {
    return(list(slots = list(vec_slice(slots[[1L]], 0L)), index = index))
  }
  list(slots = list(), index = index)
}

# The data and vecvec attributes for a vecvec of `slots` indexed by `index`,
# in canonical form. The layout attributes (dim, dimnames, names) of `index` are
# kept on the result.
vv_new_data <- function(slots, index) {
  layout <- attributes(index)
  layout <- layout[names(layout) %in% vv_layout_attrs]
  index <- as.integer(index)
  attributes(index) <- NULL
  slots <- unname(as.list(slots))

  # A single atomic slot used once, in order (as when rebuilding the result of
  # a function of a single-type vecvec's values)
  if (length(slots) == 1L && vv_can_type(slot <- slots[[1L]]) &&
      vv_is_identity(index, vec_size(slot))) {
    data <- slot
    attributes(data) <- c(layout, list(vecvec_ptype = vv_atomic_ptype(slot)))
    return(data)
  }

  n <- vv_normalise(slots, index)
  slots <- n$slots
  index <- n$index

  if (length(slots) == 1L && vv_can_type(slot <- slots[[1L]])) {
    data <- slot
    attributes(data) <- NULL
    if (!vv_is_identity(index, length(data))) data <- data[index]
    attributes(data) <- c(layout, list(vecvec_ptype = vv_atomic_ptype(slot)))
    return(data)
  }

  attributes(index) <- c(layout, list(vecvec_x = slots))
  index
}

# A vecvec like `x` (same class and other attributes) of `slots` indexed by
# `index`. The dim, dimnames and names of `index` are used for the result.
vv_rebuild <- function(x, slots, index = vv_index(x)) {
  at <- attributes(x)
  if (!is.null(at[["x"]])) {
    # From the layout of vecvec <= 1.4.0, whose (saved) class definition reads
    # `@x` from this attribute: use the current definition for plain vecvecs.
    at[["x"]] <- NULL
    if (identical(class(x)[[1L]], "vecvec::vecvec")) at[["S7_class"]] <- class_vecvec
  }
  at <- at[!names(at) %in% c(vv_layout_attrs, vv_ptype_attr, vv_slots_attr, vv_mixed_attr)]
  out <- vv_new_data(slots, index)
  attributes(out) <- c(attributes(out), at)
  out
}

# Replace the index of a vecvec (like `S7_data<-`, the dim/dimnames/names of
# `value` are used for the result).
`vv_index<-` <- function(x, value) {
  vv_rebuild(x, vv_slots(x), value)
}

# ------------------------------------------------------------------------------
# Entry points for class_vecvec
# ------------------------------------------------------------------------------
#
# Packages that extend class_vecvec keep the copy of it they were installed
# with, whose constructor, property and validator call these functions by
# name. Keep their names and arguments stable (vv_new_data(), vv_slots(),
# vv_set_slots(), vv_validate()).

# Replace the stored vectors of a vecvec, keeping its index (`@x<-`).
vv_set_slots <- function(x, value) {
  vv_rebuild(x, value, vv_index(x))
}

# Validate a vecvec.
vv_validate <- function(x) {
  vec <- vapply(vv_slots(x), vec_is, logical(1L))
  if (!all(vec)) {
    "@x must be a list of primitive vectors"
  }
}

# Copies of class_vecvec from earlier versions (in packages installed before
# vecvec was updated) call these, but store vecvecs in an older layout. Rather
# than fail with "could not find function", say which package to reinstall.
vecvec_flatten_adj <- function(x) vv_abort_outdated_class()
vv_alt <- function(i, slots) vv_abort_outdated_class()
vv_mirror <- function(x) vv_abort_outdated_class()

vv_abort_outdated_class <- function() {
  pkgs <- character()
  for (i in seq_len(sys.nframe())) {
    f <- sys.function(i)
    if (inherits(f, "S7_class")) pkgs <- c(pkgs, attr(f, "package", exact = TRUE))
  }
  pkgs <- unique(setdiff(pkgs, "vecvec"))
  version <- as.character(utils::packageVersion("vecvec"))
  if (length(pkgs)) {
    msg <- c(
      "{.pkg {pkgs}} was installed with an older version of {.pkg vecvec}.",
      "i" = "Packages copy the vecvec class when they're installed, and this copy no longer matches.",
      "i" = "Reinstall {.pkg {pkgs}} to use it with {.pkg vecvec} {version}."
    )
  } else {
    msg <- c(
      "A package extending {.pkg vecvec} was installed with an older version of it.",
      "i" = "Packages copy the vecvec class when they're installed, and this copy no longer matches.",
      "i" = "Reinstall that package to use it with {.pkg vecvec} {version}."
    )
  }
  cli::cli_abort(msg, call = NULL)
}

# `class(x, i)` for an S7 class extending class_vecvec, building a plain vecvec
# directly (as new_object() would, without validating the values of a result
# that vecvec computed itself).
vv_construct <- function(class, x, i = seq_len(sum(list_sizes(x)))) {
  if (!identical(class, class_vecvec)) return(class(x, i))
  out <- vv_new_data(x, i)
  attributes(out) <- c(attributes(out), vv_object_attrs())
  out
}

# The attributes S7 gives a plain vecvec (its class, and how S7 records the S7
# class, which depends on the version of S7). They're taken from an empty
# vecvec made in this session, once.
vv_object_attrs <- function() {
  if (is.null(vv_object_cache$attrs)) {
    at <- attributes(class_vecvec())
    vv_object_cache$attrs <- at[!names(at) %in% c(vv_layout_attrs, vv_ptype_attr, vv_slots_attr)]
  }
  vv_object_cache$attrs
}
vv_object_cache <- new.env(parent = emptyenv())
