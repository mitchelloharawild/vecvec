# ------------------------------------------------------------------------------
# Base R modelling (lm(), glm(), model.matrix(), prcomp(), ...)
# ------------------------------------------------------------------------------
#
# Modelling code reads the data of a vecvec directly in C. For a single-type
# vecvec that is the data of its values, so it works whenever that data is what
# the values mean (numbers, dates, strings). Otherwise (a mixed-type vecvec,
# whose data is an index, or a type whose data isn't its value, like a factor's
# codes) modelling errors instead of silently fitting on the wrong numbers.
#
# model.frame() calls makepredictcall() on every variable, which is where the
# error comes from for formula interfaces. Other code checks is.numeric(),
# which is only TRUE when the data can be used as numbers.

#' Is a vector's data the same as its values?
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' When all elements of a vecvec share a single type, base R functions that
#' read the vecvec's data directly in C, such as [stats::lm()],
#' [stats::model.matrix()] and [stats::prcomp()], see the data of those
#' elements. That's only correct when a vector's data *is* its value. For
#' example, a date's data is its number of days since 1970, but a factor's
#' data is a code for its level.
#'
#' `vecvec_storage_is_value()` declares this for a type. Modelling functions
#' error for single-type vecvecs whose type returns `FALSE`, as they do for
#' mixed-type vecvecs, rather than fit on the wrong numbers.
#'
#' The default is `TRUE` for vectors without a class, and `FALSE` for vectors
#' with a class, except for dates, date-times and time differences. Packages
#' that define a vector class whose data is its value can register a method
#' returning `TRUE`.
#'
#' @param x A zero-length vector of the type to check (the prototype of a
#'   vecvec's single type).
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' vecvec_storage_is_value(double())
#' vecvec_storage_is_value(Sys.Date()[0])
#' vecvec_storage_is_value(factor())
#'
#' # In a package defining the class "myclass":
#' # S3method(vecvec::vecvec_storage_is_value, myclass)
#' # vecvec_storage_is_value.myclass <- function(x) TRUE
#'
#' @export
vecvec_storage_is_value <- function(x) {
  UseMethod("vecvec_storage_is_value")
}

#' @export
vecvec_storage_is_value.default <- function(x) !is.object(x)

#' @export
vecvec_storage_is_value.Date <- function(x) TRUE

#' @export
vecvec_storage_is_value.POSIXct <- function(x) TRUE

#' @export
vecvec_storage_is_value.difftime <- function(x) TRUE

# Why the data of `x` can't be used as its values, or NULL if it can. The
# messages are cli strings to interpolate in `env`.
vv_storage_problem <- function(x) {
  env <- environment()
  if (vv_is_typed(x)) {
    ptype <- vv_typed_ptype(x)
    if (isTRUE(vecvec_storage_is_value(ptype))) return(NULL)
    type <- vec_ptype_full(ptype)
    return(list(
      what = "a vecvec of {.cls {type}}",
      why = c("i" = "Its data isn't the same as its values (see {.fn vecvec::vecvec_storage_is_value})."),
      fix = "Convert it first, e.g. with {.fn as.numeric} or {.fn as.factor}.",
      env = env
    ))
  }

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
  problem <- vv_storage_problem(var)
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

# Type-testing predicates normally check the type of the object they're given.
# A vecvec is numeric when its data can be used as numbers: when it holds a
# single numeric type whose data is its value. As for base types, a single-type
# vecvec of dates isn't numeric.
method(is.numeric, class_vecvec) <- function(x) {
  if (!vv_is_typed(x)) return(FALSE)
  ptype <- vv_typed_ptype(x)
  is.numeric(ptype) && isTRUE(vecvec_storage_is_value(ptype))
}
