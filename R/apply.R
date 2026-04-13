#' Apply a function to each vector of the vecvec
#'
#' The `vecvec_apply()` function applies a function `f` to each vector in the 
#' `vecvec` vectors.
#' 
#' @param x A vecvec object
#' @param f A function to apply to each vector
#' @param ... Additional arguments passed to `f`
#' 
#' @return A vecvec data type with the same structure as `x` but with each 
#'   vector transformed by `f`.
#' 
#' @export
vecvec_apply <- function(x, f, ...) {
  if (vec_is_empty(x)) {
    return(x)
  }
  x@x <- vecvec_flatten_adj(lapply(x@x, f, ...))

  x
}

#' Function factory for vecvec_apply
#' 
#' The `vecvec_apply_fn()` function is a function factory that creates applies
#' the function `f` to each vector and optionally simplifies the result with 
#' `[unvecvec()]`. The function matches the forms of the original function 
#' `f` and can be used to define methods for generic functions that apply to 
#' `vecvec` objects. If `f` is a primitive function, the resulting function will
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
#' @return A function that applies `f` to each vector of a `vecvec` object and
#'  optionally simplifies the result.
#' 
#' @export
vecvec_apply_fn <- function(f, ptype = NULL, SIMPLIFY = !is.null(ptype)) {
  if (is.primitive(f)) {
    fmls <- alist(x = , ... =)
    args <- syms("...")
  } else {
    fmls <- formals(f)
    args <- fmls[-1L]
    arg_missing <- vapply(args, rlang::is_missing, logical(1L))
    args[!arg_missing] <- syms(names(args)[!arg_missing])
    arg_dots <- match("...", names(args), 0L)
    names(args)[arg_dots] <- ""
    args[arg_dots] <- syms("...")
  }

  apply_sym <- sym(names(fmls)[[1L]])
  apply_fn <- rlang::new_function(
    args = fmls,
    body = expr(vecvec::vecvec_apply(!!apply_sym, !!sym("f"), !!!args)),
    env = rlang::new_environment(list(f = f), parent = rlang::caller_env())
  )

  if (SIMPLIFY) {
    body(apply_fn) <- expr(vecvec::unvecvec(!!body(apply_fn), ptype = !!ptype))
  }

  apply_fn
}

vecvec_dispatch <- function(x, ...) {
  x@x <- lapply(x@x, .Generic, ...)
  x
}
