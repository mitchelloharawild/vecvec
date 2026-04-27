register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
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
#' @param x An S7 class object that extends [class_vecvec]. Typically the
#'   class object created by [S7::new_class()] in your package, e.g.
#'   `class_myvec`.
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
vecvec_register <- function(x) {
  vctrs_exports <- getNamespaceExports(asNamespace("vctrs"))

  if (!S7_inherits(x(), class_vecvec)) {
    stop("`x` must be an S7 object extending `vecvec`", call. = FALSE)
  }

  pkg <- attr(x, "package", exact = TRUE)
  nm <- attr(x, "name", exact = TRUE)
  cls <- paste0(pkg, "::", nm)


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
    fun = vec_cast_from_vecvec
  )
  # Register vec_cast.<class>.* methods
  lapply(
    sub("^vec_cast", cls, vec_cast_generics),
    register_s3_method,
    pkg = "vctrs",
    generic = "vec_cast",
    fun = vec_cast_to_vecvec
  )

  # --------------------------------------------------------
  # vctrs::vec_ptype2() methods
  # --------------------------------------------------------
  vec_ptype2_generics <- vctrs_exports[startsWith(vctrs_exports, "vec_ptype2.")]

  # Register vec_ptype2.*.<class> methods
  lapply(
    vec_ptype2_generics,
    register_s3_method,
    pkg = "vctrs",
    class = cls,
    fun = vec_ptype2_vecvec
  )
  # Register vec_ptype2.<class>.* methods
  lapply(
    sub("^vec_ptype2", cls, vec_ptype2_generics),
    register_s3_method,
    pkg = "vctrs",
    generic = "vec_ptype2",
    fun = vec_ptype2_vecvec
  )

  invisible(NULL)
}