# ------------------------------------------------------------------------------
# Base R modelling (lm(), glm(), model.matrix(), prcomp(), ...)
# ------------------------------------------------------------------------------
#
# Modelling code reads the data of a vecvec directly in C. A single-type vecvec
# holds the data of its type, so modelling sees exactly what it would see for
# the plain vector (whether that's sensible for the type is up to the type). A
# mixed-type vecvec holds an integer index, so modelling errors for it rather
# than silently fitting on the index.
#
# model.frame() calls makepredictcall() on every variable, which is where the
# error comes from for formula interfaces.

# Why the data of `x` can't be used in a model, or NULL if it can. The messages
# are cli strings to interpolate in `env`.
vv_model_problem <- function(x) {
  if (vv_is_typed(x)) return(NULL)
  env <- environment()

  types <- vapply(vv_slots(x), vec_ptype_full, character(1L))
  n_types <- length(types)
  if (n_types == 1L) {
    # A single type that isn't held in an atomic vector (e.g. a record)
    return(list(
      what = "a vecvec of {.cls {types}}",
      why = c("i" = "Its values aren't held in an atomic vector, so its data is an index."),
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
