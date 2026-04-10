# Internal helper for casting which is less strict than `vec_cast()` for 
# user specified casts. This is used in `as.*` methods and `unvecvec()` when a
# `ptype` is supplied.
vec_cast_unsafe <- S7::new_generic("vec_cast_unsafe", c("x", "to"))
method(vec_cast_unsafe, list(class_any, class_vecvec)) <- function(x, to) {
  vecvec(x)
}
method(vec_cast_unsafe, list(class_any, class_logical)) <- function(x, to) as.logical(x)
method(vec_cast_unsafe, list(class_any, class_integer)) <- function(x, to) as.integer(x)
method(vec_cast_unsafe, list(class_any, class_double)) <- function(x, to) as.double(x)
method(vec_cast_unsafe, list(class_any, class_complex)) <- function(x, to) as.complex(x)
method(vec_cast_unsafe, list(class_any, class_character)) <- function(x, to) as.character(x)
method(vec_cast_unsafe, list(class_any, class_raw)) <- function(x, to) as.raw(x)
method(vec_cast_unsafe, list(class_any, class_Date)) <- function(x, to) as.Date(x)
method(vec_cast_unsafe, list(class_any, class_POSIXct)) <- function(x, to) as.POSIXct(x, tz = attr(to, "tzone", exact = TRUE))
method(vec_cast_unsafe, list(class_any, class_POSIXlt)) <- function(x, to) as.POSIXlt(x, tz = attr(to, "tzone", exact = TRUE))
method(vec_cast_unsafe, list(class_any, class_any)) <- function(x, to) vec_cast(x, to)

globalVariables("properties")

# Casting methods
# method(convert, list(class_any, class_vecvec)) <- function(from, to, ...) {
#   vecvec(from)
# }
# method(
#   convert,
#   list(
#     class_vecvec,
#     new_union(
#       class_vector,
#       class_Date,
#       class_POSIXct,
#       class_POSIXlt,
#       class_data.frame
#     )
#   )
# ) <- function(from, to, ...) unvecvec(from, ptype = to$constructor())

method(as.logical, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_logical$constructor())
method(as.raw, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_raw$constructor())
method(as.integer, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_integer$constructor())
# For some reason this method isn't being registered properly with S7::method<-
# method(as.double, class_vecvec) <- function(x) unvecvec(x, ptype = class_double$constructor())
#' @export
#' @method as.double vecvec::vecvec
`as.double.vecvec::vecvec` <- function(x, ...) unvecvec(x, ptype = class_double$constructor())
method(as.complex, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_complex$constructor())
method(as.character, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_character$constructor())
method(as.Date, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_Date$constructor())
method(as.data.frame, class_vecvec) <- function(x, ...) unvecvec(x, ptype = class_data.frame$constructor())
method(as.POSIXct, class_vecvec) <- function(x, tz = "", ...) {
  unvecvec(x, ptype = class_POSIXct$constructor(tz = tz))
}
method(as.POSIXlt, class_vecvec) <- function(x, tz = "", ...) {
  unvecvec(x, ptype = class_POSIXlt$constructor(tz = tz))
}
method(as.list, class_vecvec) <- function(x, ...) {
  x@x <- lapply(x@x, as.list)
  unvecvec(x, ptype = list())
}

