# ------------------------------------------------------------------------------
# Array binding (cbind / rbind / abind)
# ------------------------------------------------------------------------------

# Expand `d` to `ndim` dimensions by inserting a length-1 dim at `along` -
# how a plain vector becomes a single row/column/slice when bound into an array.
vecvec_pad_dim <- function(d, along, ndim) {
  if (length(d) == ndim) {
    return(d)
  }
  if (length(d) < ndim - 1L) {
    d <- c(d, rep(1L, ndim - 1L - length(d)))
  }
  append(d, 1L, after = along - 1L)
}

# Concatenate equal-rank arrays along `along`, by permuting it to be the
# last dimension (so concatenating column-major data does the job) then
# permuting back.
vecvec_bind_arrays <- function(arrs, along) {
  ndim <- length(dim(arrs[[1L]]))
  perm <- c(seq_len(ndim)[-along], along)
  moved <- lapply(arrs, aperm, perm = perm)
  along_sizes <- vapply(moved, function(m) dim(m)[[ndim]], integer(1L))

  out <- unlist(moved, use.names = FALSE)
  dim(out) <- c(dim(moved[[1L]])[-ndim], sum(along_sizes))
  aperm(out, order(perm))
}

# Names for the `along` dimension: each argument's own dimnames (if full-rank,
# so unaffected by padding), else its `...` name, suffixed if it spans >1 slice.
vecvec_abind_along_names <- function(dots, fdims, nms, along) {
  parts <- .mapply(
    function(x, fd, nm) {
      size <- fd[[along]]
      d <- dim(x)
      if (!is.null(d) && length(d) == length(fd)) {
        dn <- dimnames(x)
        if (!is.null(dn) && !is.null(dn[[along]])) {
          return(dn[[along]])
        }
      }
      if (nzchar(nm)) {
        if (size == 1L) nm else paste0(nm, seq_len(size))
      } else {
        rep("", size)
      }
    },
    list(dots, fdims, nms), NULL
  )

  out <- unlist(parts, use.names = FALSE)
  if (all(!nzchar(out))) NULL else out
}

# Names for non-`along` dimensions: taken from the first full-rank argument
# (so its dimnames align 1:1 with the result, unshifted by padding).
vecvec_abind_other_names <- function(dots, fdims, other) {
  lapply(other, function(k) {
    for (i in seq_along(dots)) {
      d <- dim(dots[[i]])
      if (!is.null(d) && length(d) == length(fdims[[i]])) {
        dn <- dimnames(dots[[i]])
        if (!is.null(dn) && !is.null(dn[[k]])) {
          return(dn[[k]])
        }
      }
    }
    NULL
  })
}

#' Combine vecvec arrays along an arbitrary dimension
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `vecvec_abind()` generalises [cbind()]/[rbind()] to arbitrary-dimensional
#' arrays, combining `...` along the dimension given by `along`. It underlies
#' the `cbind()`/`rbind()` methods for `vecvec` (`along = 2` and `along = 1`
#' respectively), and can also be used directly to stack higher-dimensional
#' `vecvec` arrays, e.g. combining two matrices into a 3D array with
#' `along = 3`.
#'
#' Inputs that aren't already `vecvec` objects are first wrapped with
#' [vecvec()]. An input with no `dim` (a plain vector) is treated as having a
#' single dimension of length `1` at `along`, so a bare vector becomes a
#' single column (`along = 2`), row (`along = 1`), or slice (`along > 2`) -
#' matching how [cbind()]/[rbind()] treat plain vectors. Every dimension
#' other than `along` must match (after this padding) across all inputs.
#'
#' @param ... Vectors or dim-annotated `vecvec` arrays to combine; wrapped
#'   with [vecvec()] automatically if not already `vecvec` objects. Named
#'   arguments (e.g. `vecvec_abind(a = x, b = y)`) name the corresponding
#'   position(s) along `along`, similar to [cbind()]/[rbind()].
#' @param along The dimension along which to combine `...`. Defaults to `1`
#'   (row-wise, as [rbind()]). The result always has at least
#'   `max(2, along)` dimensions, so binding plain vectors produces a matrix
#'   (like [cbind()]/[rbind()]) rather than a flat vector - use [c()] for
#'   flat concatenation.
#'
#' @return A `vecvec` array whose dimension at `along` is the sum of the
#'   `along` dimensions of `...` (after padding), and whose other dimensions
#'   match `...`.
#'
#' @seealso [vecvec()]; [c()] for flat (dim-less) concatenation of `vecvec`
#'   objects.
#'
#' @examples
#' # cbind()-like: bind columns
#' vecvec_abind(vecvec(1:3), vecvec(4:6), along = 2)
#'
#' # rbind()-like: bind rows (the default)
#' vecvec_abind(vecvec(1:3), vecvec(4:6))
#'
#' # Stack two matrices into a 3D array
#' m1 <- array(vecvec(1:6), dim = c(2, 3))
#' m2 <- array(vecvec(7:12), dim = c(2, 3))
#' vecvec_abind(m1, m2, along = 3)
#'
#' @export
vecvec_abind <- function(..., along = 1L) {
  dots <- rlang::list2(...)
  dots <- dots[!vapply(dots, is.null, logical(1L))]
  if (length(dots) == 0L) {
    return(vecvec())
  }

  nms <- names(dots) %||% character(length(dots))
  nms[is.na(nms)] <- ""
  dots <- unname(dots)
  dots <- lapply(dots, function(x) if (is_vecvec(x)) x else vecvec(x))

  dims <- lapply(dots, function(x) dim(x) %||% length(x))
  ndim <- max(2L, along, lengths(dims))

  fdims <- lapply(dims, vecvec_pad_dim, along = along, ndim = ndim)

  other <- seq_len(ndim)[-along]
  if (length(other)) {
    ref <- fdims[[1L]][other]
    bad <- !vapply(fdims, function(d) identical(d[other], ref), logical(1L))
    if (any(bad)) {
      i <- which(bad)[1L]
      cli::cli_abort(
        c(
          "All arguments to {.fn vecvec_abind} must have matching dimensions, other than {.arg along} ({along}).",
          "i" = "Argument {i} has dimensions {.val {fdims[[i]]}}, but expected {.val {ref}} at the non-{.arg along} dimensions."
        ),
        call = NULL
      )
    }
  }

  # Offset each argument's storage indices into disjoint ranges of the merged `@x` below.
  slot_counts <- vapply(dots, function(x) sum(lengths(x@x)), integer(1L))
  offsets <- c(0L, cumsum(slot_counts)[-length(dots)])
  idx_arrs <- .mapply(
    function(x, fd, off) {
      idx <- S7_data(x) + off
      dim(idx) <- fd
      idx
    },
    list(dots, fdims, offsets), NULL
  )
  combined_idx <- if (length(dots) == 1L) {
    idx_arrs[[1L]]
  } else {
    vecvec_bind_arrays(idx_arrs, along = along)
  }

  out <- class_vecvec(
    x = unlist(lapply(dots, function(x) x@x), recursive = FALSE),
    i = as.integer(combined_idx)
  )
  dim(out) <- dim(combined_idx)

  along_nms <- vecvec_abind_along_names(dots, fdims, nms, along)
  other_dn <- vecvec_abind_other_names(dots, fdims, other)
  if (!is.null(along_nms) || any(!vapply(other_dn, is.null, logical(1L)))) {
    dn <- vector("list", ndim)
    dn[[along]] <- along_nms
    dn[other] <- other_dn
    dimnames(out) <- dn
  }

  out
}

#' @method cbind vecvec::vecvec
#' @rawNamespace S3method(cbind,"vecvec::vecvec")
`cbind.vecvec::vecvec` <- function(..., deparse.level = 1) {
  vecvec_abind(..., along = 2L)
}

#' @method rbind vecvec::vecvec
#' @rawNamespace S3method(rbind,"vecvec::vecvec")
`rbind.vecvec::vecvec` <- function(..., deparse.level = 1) {
  vecvec_abind(..., along = 1L)
}
