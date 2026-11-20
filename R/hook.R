register_s3_method <- function(pkg, generic, class, fun = NULL) {
  if (!is.character(pkg) || length(pkg) != 1) {
    cli::cli_abort("{.arg pkg} must be a single string.", call = NULL)
  }
  if (!is.character(generic) || length(generic) != 1) {
    cli::cli_abort("{.arg generic} must be a single string.", call = NULL)
  }
  if (!is.character(class) || length(class) != 1) {
    cli::cli_abort("{.arg class} must be a single string.", call = NULL)
  }

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else if (!is.function(fun)) {
    cli::cli_abort("{.arg fun} must be a function.", call = NULL)
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

#' Register methods for a vecvec subclass
#'
#' Call `vecvec_register()` inside your package's `.onLoad()` function to
#' register the S3 methods that allow a custom
#' [vecvec][vecvec::class_vecvec] subclass to participate in the
#' [vctrs](https://vctrs.r-lib.org/) vector system.
#'
#' Specifically, `vecvec_register()` registers:
#' - `vec_cast.<class>.*` methods, so that other types can be cast *to* your
#'   vecvec subclass.
#' - `vec_cast.*.<class>` methods, so that your vecvec subclass can be cast
#'   *from* other types.
#'
#' With `single_type = TRUE`, vctrs treats a single-type vector of your
#' subclass as a vector of its type (as for [vecvec()]), which makes vctrs
#' operations on it (slicing, combining, matching, ...) about as fast as on
#' the plain vector. The common type of two vectors is then single-type only
#' when both have that type, vctrs casts to a single-type vector cast the
#' values to its type, and any `vec_cast()` method you register between two
#' vectors of your subclass must return the result of [vecvec_cast()].
#' Otherwise all vectors of your subclass are one vctrs type.
#'
#' @param x An S7 class object that extends [class_vecvec]. Typically the
#'   class object created by [S7::new_class()] in your package, e.g.
#'   `class_myvec`.
#' @param f_ptype2 The function to register as the `vec_ptype2` method for this
#'  class. Defaults to `vec_ptype2_vecvec`, guarantees that the common type of
#'  two vectors is a vecvec vector.
#' @param f_cast_to The function to register as the `vec_cast.<class>.*` methods
#'  for this class. Defaults to `vec_cast_to_vecvec`, which attempts to cast 
#'  other types to vecvec by calling `vecvec()`.
#' @param f_cast_from The function to register as the `vec_cast.*.<class>` 
#'  methods for this class. Defaults to `vec_cast_from_vecvec`, which attempts 
#'  to cast vecvec to other types by calling `unvecvec()`.
#'
#' @param single_type Whether vctrs treats single-type vectors of the class as
#'  vectors of their type (see Details).
#'
#' @return `NULL`, invisibly. Called for its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In your package, define a vecvec subclass:
#' class_myvec <- S7::new_class(
#'   "myvec",
#'   parent = vecvec::class_vecvec
#' )
#'
#' # Then register it in .onLoad():
#' .onLoad <- function(libname, pkgname) {
#'   S7::methods_register()
#'   vecvec::vecvec_register(class_myvec)
#' }
#' }
vecvec_register <- function(
  x, f_ptype2 = vec_ptype2_vecvec, 
  f_cast_to = vec_cast_to_vecvec, f_cast_from = vec_cast_from_vecvec,
  single_type = FALSE
) {
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))

  if (!S7_inherits(x(), class_vecvec)) {
    cli::cli_abort(
      c(
        "{.arg x} must be an S7 object extending {.cls vecvec}.",
        "i" = "Create a subclass with {.fn S7::new_class}, setting {.code parent = vecvec::class_vecvec}."
      ),
      call = NULL
    )
  }

  pkg <- attr(x, "package", exact = TRUE)
  nm <- attr(x, "name", exact = TRUE)
  cls <- paste0(pkg, "::", nm)
  vv_single_type_classes[[cls]] <- isTRUE(single_type)


  # --------------------------------------------------------
  # vctrs::vec_cast() methods
  # --------------------------------------------------------
  vec_cast_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_cast.")]

  # Register vec_cast.*.<class> methods
  lapply(
    vec_cast_generics,
    register_s3_method,
    pkg = "vctrs",
    class = cls,
    fun = f_cast_from
  )
  # Register vec_cast.<class>.* methods. vctrs needs the result to have the
  # form of `to` (see vecvec_cast()).
  cast_to <- function(x, to, ...) vecvec_cast(f_cast_to(x, to, ...), to)
  lapply(
    sub("^vec_cast", cls, vec_cast_generics),
    register_s3_method,
    pkg = "vctrs",
    generic = "vec_cast",
    fun = cast_to
  )

  # Register vec_cast.<class>.<class>.
  register_s3_method(
    pkg = "vctrs",
    generic = "vec_cast",
    class = paste0(cls, ".", cls),
    fun = vec_cast_same_vecvec
  )

  # --------------------------------------------------------
  # vctrs::vec_ptype2() methods
  # --------------------------------------------------------
  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]
  # A common type is single-type only when all the vecvecs have that type
  ptype2 <- function(x, y, ...) vv_ptype2_form(f_ptype2(x, y, ...), x, y)

  # Register vec_ptype2.*.<class> methods
  lapply(
    vec_ptype2_generics,
    register_s3_method,
    pkg = "vctrs",
    class = cls,
    fun = ptype2
  )
  # Register vec_ptype2.<class>.* methods
  lapply(
    sub("^vec_ptype2", cls, vec_ptype2_generics),
    register_s3_method,
    pkg = "vctrs",
    generic = "vec_ptype2",
    fun = ptype2
  )
  # Register vec_ptype2.<class>.<class> so s3 fallback is not triggered
  register_s3_method(
    pkg = "vctrs",
    generic = "vec_ptype2",
    class = paste0(cls, ".", cls),
    fun = ptype2
  )

  invisible(NULL)
}