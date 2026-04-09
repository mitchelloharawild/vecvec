#' Create a new vector of vectors
#'
#' @param ... Vectors to combine into a single vector without type coercion.
#' @param class Name of subclass.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @seealso [unvecvec()] coerces the mixed-type vector into a single-typed
#' regular vector.
#'
#' @examples
#' vecvec(Sys.Date(), rnorm(3), letters)
#'
#' @export
vecvec <- function(...) {
  class_vecvec(x = rlang::list2(...))
}

#' Obtain a singular common vector type from a vector of vectors
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x A vecvec to unvecvec (convert to its underlying vector type)
#'
#' @return A simple vector, all containing the same type of data.
#'
#' @export
unvecvec <- function(x, ..., ptype = NULL) {
  if ((len <- length(x)) == 0L) return(unlist(x@x))

  # Cast mixed vector types to common type
  if (is.null(ptype)) {
    ptype <- vec_ptype_common(!!!x@x) %||% logical()
  }
  x@x <- lapply(x@x, vec_cast, to = ptype)

  # Construct output single-typed vector
  res <- vec_init(ptype, n = len)
  res[x@i[!is.na(x@i)]] <- vec_c(!!!x@x)
  res
}

# Class accessors

#' Test if an object is a vecvec
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `class_vecvec`, `FALSE` otherwise.
#'
#' @seealso [vecvec()] to create a vecvec object.
#'
#' @examples
#' vv <- vecvec(1:3, letters)
#' is_vecvec(vv)
#' is_vecvec(1:3)
#'
#' @export
is_vecvec <- function(x) S7_inherits(x, class_vecvec)

# Display methods
method(print, class_vecvec) <- function(x, ...) {
  vctrs::obj_print(x, ...)
}
method(format, class_vecvec) <- function(x, ...) {
  vec_c(!!!lapply(x@x, format, ...), .ptype = character())[x@i]
}


# Attribute methods
method(length, class_vecvec) <- function(x) length(x@i)

# Indexing methods
method(`[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  len <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  pos <- findInterval(idx[!is.na(idx)], len, left.open = TRUE)
  grp <- vec_group_loc(pos)
  
  # TODO - if grp has only one group we can return a simpler vecvec type

  # Prune unused vectors
  x@x <- .mapply(
    function(key, loc) x@x[[key]][idx[loc] - len[key], drop = FALSE],
    grp, NULL
  )

  # Update indices
  idx[!is.na(pos)] <- unlist(grp$loc)
  x@i <- idx

  x
}
method(`[[`, class_vecvec) <- function(x, i, ...) {
  idx <- x@i[i]
  len <- c(0L, cumsum(lengths(x@x[-length(x@x)])))
  pos <- findInterval(idx, len)
  x@x[[pos]][[idx - len[pos]]]
}

# Casting methods
method(convert, list(class_any, class_vecvec)) <- function(from, to, ...) {
  vecvec(from)
}
method(convert, 
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
method(as.list, class_vecvec) <- function(x) convert(x, class_list)
method(as.Date, class_vecvec) <- function(x) convert(x, class_Date)
method(as.POSIXct, class_vecvec) <- function(x) convert(x, class_POSIXct)
method(as.POSIXlt, class_vecvec) <- function(x) convert(x, class_POSIXlt)
method(as.data.frame, class_vecvec) <- function(x) convert(x, class_data.frame)

# Combining methods
method(c, class_vecvec) <- function(..., recursive = FALSE) {
  dots <- rlang::list2(...)
  is_vv <- vapply(dots, is_vecvec, logical(1L))
  dots[!is_vv] <- lapply(dots[!is_vv], vecvec)

  # TODO - reduce structure into a common vecvec
  i_offsets <- cumsum(vapply(
    dots[-length(dots)],
    function(x) sum(lengths(x@x)),
    integer(1L)
  ))

  x <- lapply(dots, function(x) x@x)
  i <- c(dots[[1L]]@i, .mapply(
    function(x, j) x@i + j,
    list(x = dots[-1L], j = i_offsets), NULL
  ))

  class_vecvec(
    x = unlist(x, recursive = FALSE),
    i = unlist(i, recursive = FALSE)
  )
}