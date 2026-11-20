#' Create a vector of vectors
#'
#' @description
#' 
#' `r lifecycle::badge('stable')`
#'
#' A `vecvec` is a vector that holds elements of different types without
#' coercing them to a common type. Unlike a list of vectors, a `vecvec` behaves 
#' as a flat vector (hence vector of vectors). This means that you can 
#' operations (such as indexing, arithmetic, and statistics) apply across all
#' elements of a `vecvec` as if they were combined into a single vector.
#'
#' Mixed-type vectors can be useful if you need to store heterogeneous data in a
#' vector-like structure. In most cases, I believe this is bad practice for data
#' analytics, but this could be useful for tidying up messy data. The most
#' valuable use case for vecvec is as a data structure for mixed-type semantic
#' vectors. This package is used by [mixtime](https://pkg.mitchelloharawild.com/mixtime/)
#' and [distributional](https://pkg.mitchelloharawild.com/distributional/) to 
#' create vectors of time with different chronons and distributions with 
#' different shapes.
#'
#' To convert a `vecvec` back to a plain typed vector, use [unvecvec()], which
#' casts all elements to a common type via [vctrs::vec_cast()].
#'
#' @param ... Vectors to combine. Each vector is stored as a separate typed
#'   slot; no type coercion is performed.
#'
#' @return A `vecvec` object whose length equals the total number of elements
#'   across all input vectors.
#'
#' @seealso [unvecvec()] to coerce a `vecvec` to a single-typed vector;
#'   [is_vecvec()] to test whether an object is a `vecvec`.
#'
#' @examples
#' # Mixed types are preserved without coercion
#' vv <- vecvec(Sys.Date(), rnorm(3), letters)
#' vv
#'
#' # .[i] Indexing works like a flat vector
#' vv[c(1L, 3L, 7L)]
#' 
#' # .[[i]] drops to the original vector type
#' vv[[2L]]
#'
#' @export
vecvec <- function(...) {
  class_vecvec(x = rlang::list2(...))
}

#' Coerce a vector of vectors to a single typed vector
#'
#' @description
#' 
#' `r lifecycle::badge('stable')`
#'
#' Flattens a `vecvec` into a plain R vector by casting all elements to a
#' common type. This is the inverse of [vecvec()].
#'
#' Type resolution follows vctrs coercion rules: when `ptype` is `NULL` the
#' common type is determined automatically from the slots of `x` via
#' [vctrs::vec_ptype_common()]. If no common type can be found (e.g. all slots
#' are empty), the result falls back to `logical()`.
#'
#' @param x A `vecvec` object.
#' @param ptype A prototype specifying the desired output type, e.g.
#'   `character()` or `numeric()`. If `NULL` (the default), the common type is
#'   inferred from the elements of `x` using [vctrs::vec_ptype_common()],
#'   falling back to `logical()` when no common type can be determined. Passing
#'   an explicit `ptype` is useful when you need a guaranteed output type
#'   regardless of what `x` contains, or when automatic inference would pick an
#'   undesirable type.
#'
#' @return A vector of length `length(x)` and type `ptype` (or the inferred
#'   common type when `ptype = NULL`). Positions corresponding to `NA` indices
#'   in the underlying `vecvec` structure are filled with `NA`.
#'
#' @seealso [vecvec()] to create a `vecvec`; [vctrs::vec_ptype_common()] for
#'   the type inference rules; [vctrs::vec_cast()] for the casting rules.
#'
#' @examples
#' vv <- vecvec(1:3, c(4.5, 5.5))
#'
#' # Automatic type inference: integer + double -> double
#' unvecvec(vv)
#'
#' # Force a specific output type
#' unvecvec(vv, ptype = character())
#'
#' @export
unvecvec <- function(x, ptype = NULL) {
  if (!is_vecvec(x)) {
    cli::cli_abort(
      c(
        "{.arg x} must be a {.cls vecvec} object.",
        "i" = "Create one with {.fn vecvec}."
      ),
      call = NULL
    )
  }

  # Single-type: the values are already of one type
  if (vv_is_typed(x)) {
    slot <- vv_slots(x)[[1L]]
    return(if (is.null(ptype)) slot else vec_cast_unsafe(slot, to = ptype))
  }

  slots <- vv_slots(x)
  if ((len <- length(x)) == 0L) {
    return(if (is.null(ptype)) vec_c(!!!slots) else vec_slice(ptype, 0L))
  }

  # Cast mixed vector types to common type
  if (is.null(ptype)) {
    ptype <- vec_ptype_common(!!!slots) %||% logical()
    # Use safe casting from vctrs
    vec_cast_unsafe <- vec_cast
  }
  slots <- lapply(slots, vec_cast_unsafe, to = ptype)

  # Construct output single-typed vector
  idx <- vv_index(x)
  attributes(idx) <- NULL
  res <- vec_init(ptype, n = len)
  pos <- !is.na(idx)
  if (any(pos)) {
    res <- vec_assign(res, pos, vec_slice(vec_c(!!!slots), idx[pos]))
  }
  res
}

# ------------------------------------------------------------------------------
# Display methods
# ------------------------------------------------------------------------------
method(print, class_vecvec) <- function(x, max = getOption("max.print"), ...) {
  d <- dim(x)
  cat("<", vec_ptype_full(x), "[", paste0(d %||% length(x), collapse = ","), "]>\n", sep = "")

  n <- length(x)
  if (n > 0L) {
    n_show <- min(n, max)

    if (!is.null(d)) {
      if (n_show < n) {
        # Map the first n_show display-order (row-major) positions to their
        # column-major storage indices by permuting the first two dimensions.
        cm_idx <- aperm(array(seq_len(n), d), c(2L, 1L, seq_along(d)[-(1:2)]))[seq_len(n_show)]
        fmt_arr <- character(n)
        fmt_arr[cm_idx] <- format(x[cm_idx])
        dim(fmt_arr) <- d
        dimnames(fmt_arr) <- dimnames(x)
        print(fmt_arr, quote = FALSE, max = n_show)
      } else {
        print(format(x), quote = FALSE, max = max)
      }
    } else {
      # Vector: subset before formatting to avoid materialising all n elements.
      print(format(x[seq_len(n_show)]), quote = FALSE)
      if (n > max) {
        cat(sprintf(
          '[ reached \'max\' / getOption("max.print") -- omitted %d entries ]\n',
          n - max
        ))
      }
    }
  }

  invisible(x)
}
method(format, class_vecvec) <- function(x, ...) {
  fmt <- vec_c(!!!lapply(x@x, format, ...), .ptype = character())[vv_index(x)]
  dim(fmt) <- dim(x)
  dimnames(fmt) <- dimnames(x)
  fmt
}

# ------------------------------------------------------------------------------
# Attribute methods
# ------------------------------------------------------------------------------
# @method length<- vecvec::vecvec
#' @rawNamespace S3method(`length<-`,"vecvec::vecvec")
`length<-.vecvec::vecvec` <- function(x, value) x[seq_len(value)]

#' Test if an object is a vecvec
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `class_vecvec`, `FALSE` otherwise.
#'
#' @seealso [vecvec()] to create a vecvec object.
#'
#' @examples
#' vv <- vecvec(1:3, letters)
#' is_vecvec(vv)
#' is_vecvec(1:3)
#'
#' @export
# `inherits()` on the S7 class vector is equivalent to S7_inherits() here and
# avoids S7's per-call overhead; this check runs once per input in c().
is_vecvec <- function(x) inherits(x, "vecvec::vecvec")

# ------------------------------------------------------------------------------
# Indexing methods
# ------------------------------------------------------------------------------
method(`[`, class_vecvec) <- function(x, i, ...) {
  # Single-type: subset the values directly
  if (vv_is_typed(x)) {
    return(vv_with_data(x, vv_data(x)[i, ...]))
  }

  # Mixed-type: subset the index, keeping only the stored values it uses
  idx <- vv_index(x)[i, ...]
  ii <- idx
  attributes(ii) <- NULL
  cmp <- vv_compact(vv_slots(x), ii)
  attributes(cmp$index) <- attributes(idx)
  vv_rebuild(x, cmp$slots, cmp$index)
}

method(`[[`, class_vecvec) <- function(x, i, ...) {
  at <- vecvec_locate(x@x, vv_index(x)[i])
  x@x[[at$slot]][[at$within]]
}

# ------------------------------------------------------------------------------
# Combining methods
# ------------------------------------------------------------------------------
method(c, class_vecvec) <- function(..., recursive = FALSE) {
  dots <- unname(rlang::list2(...))
  is_vv <- vapply(dots, is_vecvec, logical(1L))
  dots[!is_vv] <- lapply(dots[!is_vv], vecvec)

  # Single-type inputs of the same type: combine their values
  ptype <- vv_typed_ptype(dots[[1L]])
  if (!is.null(ptype) && all(vapply(dots, function(v) !is.null(p <- vv_typed_ptype(v)) && vv_same_type(p, ptype), logical(1L)))) {
    data <- lapply(dots, function(v) {
      d <- vv_data(v)
      attr(d, "dim") <- NULL
      attr(d, "dimnames") <- NULL
      d
    })
    return(vv_with_data(dots[[1L]], do.call(base::c, data)))
  }

  # Read each input's slots once through the cheap internal accessor (`@x`
  # dispatches through S7), then offset each input's indices past the slots of
  # the inputs before it. Collect indices as list elements so that a single
  # zero-length input still unlists to integer() rather than NULL.
  x <- lapply(dots, vv_slots)
  n_x <- vapply(x, function(s) sum(list_sizes(s)), integer(1L))
  offset <- c(0L, cumsum(n_x)[-length(n_x)])
  i <- .mapply(function(v, j) vv_index(v) + j, list(dots, offset), NULL)

  S7_class(..1)(
    x = unlist(x, recursive = FALSE),
    i = as.integer(unlist(i, use.names = FALSE))
  )
}

# ------------------------------------------------------------------------------
# Array methods
# ------------------------------------------------------------------------------

# @method as.vector vecvec::vecvec
#' @rawNamespace S3method(as.vector,"vecvec::vecvec")
`as.vector.vecvec::vecvec` <- function(x, mode = "any") x

# @method dim<- vecvec::vecvec
#' @rawNamespace S3method(`dim<-`,"vecvec::vecvec")
`dim<-.vecvec::vecvec` <- function(x, value) {
  attr(x, "dim") <- value
  x
}
# @method dimnames<- vecvec::vecvec
#' @rawNamespace S3method(`dimnames<-`,"vecvec::vecvec")
`dimnames<-.vecvec::vecvec` <- function(x, value) {
  attr(x, "dimnames") <- value
  x
}