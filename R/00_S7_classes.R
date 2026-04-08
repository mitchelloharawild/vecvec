#' Construct a vector of vectors
#'
#' new_vecvec() constructs a new vector of vectors from a list of vectors. It is meant to be performant, and does not check the inputs for correctness in any way. It is only safe to use after a call to df_list(), which collects and validates the columns used to construct the data frame.
#'
#' @param x An unnamed list of arbitrary vectors.
#' @param loc A named list of value locations, with `i` identifying the vector index and `x` identifying the value index. By default, the order of appearance in `x` will be used.
#' @param class Name of subclass.
#'
#' @return A vector of vectors of class `vecvec`.
#'
#' @examples
#' # Create a vecvec prototype
#' new_vecvec()
#'
#' # Construct a vecvec from a list of vectors
#' new_vecvec(list(letters, rnorm(10)))
#'
#' # Fully specify a vecvec with locations
#' new_vecvec(
#'   x = list(letters, rnorm(10)),
#'   loc = list(
#'     i = c(rep(1L, 3), rep(2L, 5), rep(1L, 23), rep(2L, 5)),
#'     x = c(1:3, 1:5, 26:4, 6:10)
#'   )
#' )
#'
#' @export
class_vecvec <- new_class(
  "vecvec",
  properties = list(
    x = class_list,
    i = class_integer
  ),
  constructor = function(x = list(), i = seq_len(sum(lengths(x)))) {
    # TODO - adjacent common vectors can be merged for efficiency
    x
    i
    new_object(S7_object(), x = x, i = i)
  },
  validator = function(self) {
    vec <- vapply(self@x, is.vector, logical(1L))
    if (!all(vec)) {
      "@x must be a list of primitive vectors"
    }
  }
)
S7::S4_register(class_vecvec)

# Alternative candidate implementation - indices in base object.
# class_vecvec <- S7::new_class(
#   "vecvec",
#   properties = list(
#     x = S7::class_list
#   ),
#   constructor = function(.data = seq_len(sum(lengths(x))), x = list()) {
#     .data
#     x
#     S7::new_object(class_integer, .data = .data, x = x)
#   },
#   validator = function(self) {
#     vec <- vapply(self@x, is.vector, logical(1L))
#     if (!all(vec)) {
#       "@x must be a list of vectors"
#     }
#   }
# )
