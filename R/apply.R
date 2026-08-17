#' Apply a function to each vector of the vecvec
#'
#' The `vecvec_apply()` function applies a function `.f` to each vector in the 
#' `vecvec` vectors.
#' 
#' @param x A vecvec object
#' @param .f A function to apply to each vector
#' @param ... Additional arguments passed to `.f`
#' 
#' @return A vecvec data type with the same structure as `x` but with each 
#'   vector transformed by `.f`.
#' 
#' @export
vecvec_apply <- function(x, .f, ...) {
  if (vec_is_empty(x)) {
    return(x)
  }
  x@x <- vecvec_flatten_adj(lapply(x@x, .f, ...))

  x
}

#' Function factory for vecvec_apply
#' 
#' The `vecvec_apply_fn()` function is a function factory that creates applies
#' the function `.f` to each vector and optionally simplifies the result with 
#' `[unvecvec()]`. The function matches the forms of the original function 
#' `.f` and can be used to define methods for generic functions that apply to 
#' `vecvec` objects. If `.f` is a primitive function, the resulting function will
#' have a apply over an argument `x` and pass through `...`.
#' 
#' @inheritParams vecvec_apply
#' @param ptype A prototype to simplify to. If `NULL`, the result will be a
#'   `vecvec` object. If not `NULL`, the result will be simplified to the type 
#'   of `ptype` if possible.
#' @param SIMPLIFY If `TRUE`, the `[unvecvec()]` will be applied to the result 
#'   of `vecvec_apply()`, using `ptype` as the target type. If `FALSE`, the 
#'   result will always be a `vecvec` object.
#' 
#' @return A function that applies `.f` to each vector of a `vecvec` object and
#'  optionally simplifies the result.
#' 
#' @export
vecvec_apply_fn <- function(.f, ptype = NULL, SIMPLIFY = !is.null(ptype)) {
  fmls <- formals(args(.f))
  args <- names(fmls)[-1L]
  args <- `names<-`(syms(args), args)
  # Remove name of `...` if it exists
  arg_dots <- match("...", names(args), 0L)
  names(args)[arg_dots] <- ""

  apply_sym <- sym(names(fmls)[[1L]])
  apply_fn <- rlang::new_function(
    args = fmls,
    body = expr(vecvec::vecvec_apply(!!apply_sym, !!sym(".f"), !!!args)),
    env = rlang::new_environment(list(.f = .f), parent = rlang::caller_env())
  )

  if (SIMPLIFY) {
    body(apply_fn) <- expr(vecvec::unvecvec(!!body(apply_fn), ptype = !!ptype))
  }

  apply_fn
}

#' Apply a vectorised function across multiple vectors of a vecvec, batched
#' by shared underlying storage slots
#'
#' The `vecvec_mapply()` function applies a function `.f` across the
#' vectors in `.l`, combining the results into a vecvec that preserves the
#' original (possibly heterogeneous) type of each element of any `vecvec`
#' inputs. Unlike `mapply()`/`Map()`, `.f` is not called once per logical
#' element - it is called once per contiguous run of positions that draw
#' from the same combination of underlying storage slots across every
#' vector in `.l` (the same slot-grouping used internally for
#' `Ops.vecvec::vecvec`, e.g. `+`/`==`). Each call therefore receives one
#' vector per element of `.l` (that group's values) rather than one scalar
#' per element of `.l`, so `.f` must itself be vectorised - an ordinary
#' operator or vectorised function (e.g. `+`, `paste0`) qualifies
#' automatically, but `.f` must return a result the same length as its
#' inputs, or `vecvec_mapply()` errors.
#'
#' Because the number of calls to `.f` scales with the number of distinct
#' slot combinations rather than with `length(.l[[1]])`, this is cheap for
#' structured or replicated inputs (few distinct slot combinations) and
#' degrades toward one call per element only in the worst case (every
#' position drawing from a different combination of slots) - the same cost
#' profile as [vecvec_apply()] and `Ops.vecvec::vecvec`, and unlike a plain
#' `mapply()`/`Map()` call, which always calls `.f` once per element.
#'
#' @param .l A list of vectors to apply `.f` over. Elements of `.l` may be
#'   `vecvec` objects or plain vectors; plain vectors are treated as if
#'   wrapped with [vecvec()]. All vectors in `.l` are recycled to a common
#'   size following [vctrs::vec_recycle_common()] rules.
#' @param .f A vectorised function, applied once per group of positions
#'   that share the same underlying storage slots across every vector in
#'   `.l`. Called with one vector per element of `.l` (that group's values)
#'   plus `...`.
#' @param ... Additional arguments passed to every call of `.f`.
#' @param ptype A prototype to simplify the result to with [unvecvec()]. If
#'   `NULL` and `SIMPLIFY` is `TRUE`, the common type is inferred from the
#'   result as in [unvecvec()].
#' @param SIMPLIFY If `TRUE`, [unvecvec()] is applied to the result, using
#'   `ptype` as the target type. If `FALSE`, the result is always a `vecvec`
#'   object.
#'
#' @return A `vecvec` object combining the results of calling `.f` on each
#'   group of positions of `.l`, or (if `SIMPLIFY` is `TRUE`) that result
#'   simplified to a single type with [unvecvec()]. Positions where any
#'   input has a missing (`NA`) index form their own group and are not
#'   passed to `.f` at all - they come back as `NA` directly, matching how
#'   missing elements are treated elsewhere (e.g. `vecvec_apply()`, `[<-`,
#'   `is.na<-`).
#'
#' @seealso [vecvec_apply()] for the single-input equivalent, applying `.f`
#'   once per underlying storage vector of a single `vecvec`.
#'   `Ops.vecvec::vecvec` uses the same slot-grouping strategy for
#'   arithmetic/comparison operators.
#'
#' @export
vecvec_mapply <- function(.l, .f, ..., ptype = NULL, SIMPLIFY = !is.null(ptype)) {
  if (length(.l) == 0L) {
    cli::cli_abort("{.arg .l} must contain at least one vector.", call = NULL)
  }

  is_vv <- vapply(.l, is_vecvec, logical(1L))
  .l[!is_vv] <- lapply(.l[!is_vv], vecvec)
  .l <- vec_recycle_common(!!!.l)
  n <- vec_size(.l[[1L]])

  # Group positions by their underlying combination of storage slots (shared
  # with Ops.vecvec::vecvec), so `.f` can be called once per group on whole
  # vectors rather than once per position.
  al <- vecvec_align(.l)
  within <- al$within
  groups <- al$groups

  # A group whose slot is NA (some input is missing at that position) isn't
  # backed by a real value in any slot, so it's skipped here and mapped to a
  # missing index below instead of being passed to `.f`.
  computed <- lapply(groups, function(pos) {
    s_idx <- lapply(al$slot, `[[`, pos[1L])
    if (anyNA(s_idx)) {
      return(NULL)
    }

    group_vals <- lapply(seq_along(.l), function(j) {
      .l[[j]]@x[[s_idx[[j]]]][within[[j]][pos]]
    })
    val <- do.call(.f, c(group_vals, list(...)))

    if (length(val) != length(pos)) {
      cli::cli_abort(
        c(
          "{.arg .f} must return a result the same length as the group of positions it was called on.",
          "i" = "Got a result of length {length(val)} for a group of length {length(pos)}.",
          "i" = "{.fn vecvec_mapply} calls {.arg .f} once per group of positions that share the same underlying storage slots (like {.fn Ops.vecvec}), not once per element, so {.arg .f} must be vectorised."
        ),
        call = NULL
      )
    }
    val
  })

  is_na_group <- vapply(computed, is.null, logical(1L))
  result_x <- unname(computed[!is_na_group])

  # Precompute per-slot offsets once, then scatter each group's results into
  # result_i by position (mirrors Ops.vecvec::vecvec's construction).
  offsets <- if (length(result_x)) c(0L, cumsum(lengths(result_x))[-length(result_x)]) else integer(0L)
  result_i <- rep(NA_integer_, n)
  vi <- 0L
  for (s in seq_along(groups)) {
    if (is_na_group[[s]]) next
    vi <- vi + 1L
    result_i[groups[[s]]] <- offsets[[vi]] + seq_along(groups[[s]])
  }

  out <- class_vecvec(x = result_x, i = result_i)

  if (SIMPLIFY) {
    return(unvecvec(out, ptype = ptype))
  }
  out
}

vecvec_dispatch <- function(x, ...) {
  x@x <- lapply(x@x, .Generic, ...)
  x
}
