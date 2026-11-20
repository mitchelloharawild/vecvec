# ------------------------------------------------------------------------------
# Base R modelling (lm(), glm(), model.matrix(), prcomp(), ...)
# ------------------------------------------------------------------------------
#
# Modelling code reads the data of a vecvec directly in C. A single-type vecvec
# holds the data of its type, so modelling sees exactly what it would see for
# the plain vector (whether that's sensible for the type is up to the type). A
# mixed-type vecvec holds an integer index, so modelling errors for it rather
# than silently fitting on the index. Factors also error: modelling classifies
# them by class, which a vecvec doesn't carry (see vv_model_problem()).
#
# model.frame() calls makepredictcall() on every variable, which is where the
# error comes from for formula interfaces.

# Why the data of `x` can't be used in a model, or NULL if it can. The messages
# are cli strings to interpolate in `env`.
vv_model_problem <- function(x) {
  env <- environment()
  if (vv_is_typed(x)) {
    ptype <- vv_typed_ptype(x)
    # Models treat factors by their class (contrasts from their levels), which
    # a vecvec doesn't carry, so they would fit on the integer codes. This is
    # the only type that base modelling classifies differently from its data.
    if (!is.factor(ptype)) return(NULL)
    # (The full type label of a factor is a hash of its levels.)
    return(list(
      what = if (is.ordered(ptype)) "a vecvec of ordered factors" else "a vecvec of factors",
      why = c("i" = "Models treat factors by their class, which a vecvec doesn't have."),
      fix = "Convert it first, e.g. with {.fn unvecvec}.",
      env = env
    ))
  }

  types <- vapply(vv_slots(x), vec_ptype_full, character(1L))
  n_types <- length(types)
  if (n_types == 1L) {
    # A single type that isn't held in an atomic vector (e.g. a record), or
    # one held as an index (from a cast to a mixed-type vecvec)
    why <- if (vv_can_type(vv_slots(x)[[1L]])) {
      "It's held as an index into its values, as when cast to a mixed-type vecvec."
    } else {
      "Its values aren't held in an atomic vector, so its data is an index."
    }
    return(list(
      what = "a vecvec of {.cls {types}}",
      why = c("i" = why),
      fix = "Convert it first, e.g. with {.fn unvecvec}.",
      env = env
    ))
  }
  list(
    what = "a mixed-type vecvec",
    why = if (n_types == 0L) {
      c("i" = "It holds no values of any type.")
    } else if (anyDuplicated(types)) {
      # Types can share a label (e.g. differing only in attributes)
      c("i" = "It holds values of {n_types} different types.")
    } else {
      types <- vapply(types, function(t) cli::format_inline("{.cls {t}}"), character(1L))
      c("i" = "It holds values of {n_types} different types: {types}.")
    },
    fix = "Models need values of a single type. Convert it first, e.g. with {.fn unvecvec}.",
    env = env
  )
}

#' @importFrom stats makepredictcall
#' @rawNamespace S3method(makepredictcall,"vecvec::vecvec")
`makepredictcall.vecvec::vecvec` <- function(var, call) {
  problem <- vv_model_problem(var)
  if (!is.null(problem)) {
    problem$env$var_name <- paste(deparse(call, width.cutoff = 500L), collapse = " ")
    cli::cli_abort(
      c(
        paste0("Can't use {.var {var_name}} in a model: it is ", problem$what, "."),
        problem$why,
        "i" = problem$fix
      ),
      call = NULL,
      .envir = problem$env
    )
  }
  NextMethod()
}

# A single-type vecvec is numeric when its type is. A mixed-type vecvec isn't,
# since its data is an index.
method(is.numeric, class_vecvec) <- function(x) {
  vv_is_typed(x) && is.numeric(vv_typed_ptype(x))
}
