# Casting methods
method(convert, list(class_any, class_vecvec)) <- function(from, to, ...) {
  vecvec(from)
}
method(
  convert,
  list(
    class_vecvec,
    new_union(
      class_vector,
      class_Date,
      class_POSIXct,
      class_POSIXlt,
      class_data.frame
    )
  )
) <- function(from, to, ...) unvecvec(from, ptype = to$constructor())

method(as.logical, class_vecvec) <- function(x) convert(x, class_logical)
method(as.raw, class_vecvec) <- function(x) convert(x, class_raw)
method(as.integer, class_vecvec) <- function(x) convert(x, class_integer)
# For some reason this method isn't being registered properly with S7::method<-
# method(as.double, class_vecvec) <- function(x) convert(x, class_double)
#' @export
#' @method as.double vecvec::vecvec
`as.double.vecvec::vecvec` <- function(x) convert(x, class_double)
method(as.complex, class_vecvec) <- function(x) convert(x, class_complex)
method(as.character, class_vecvec) <- function(x) convert(x, class_character)
method(as.Date, class_vecvec) <- function(x) convert(x, class_Date)
method(as.data.frame, class_vecvec) <- function(x) convert(x, class_data.frame)
method(as.POSIXct, class_vecvec) <- function(x, tz = "", ...) {
  x@x <- lapply(x@x, as.POSIXct, tz = tz, ...)
  unvecvec(x, ptype = class_POSIXct)
}
method(as.POSIXlt, class_vecvec) <- function(x, tz = "", ...) {
  x@x <- lapply(x@x, as.POSIXlt, tz = tz, ...)
  unvecvec(x, ptype = class_POSIXlt)
}
method(as.list, class_vecvec) <- function(x) {
  x@x <- lapply(x@x, as.list)
  unvecvec(x, ptype = list())
}