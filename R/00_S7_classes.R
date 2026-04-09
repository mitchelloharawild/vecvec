#' S7 class for vecvec
#'
#' `class_vecvec()` constructs a new vecvec object from a list of vectors. It is
#' designed to be performant with minimal checks on the inputs. Direct use of
#' `class_vecvec()` is useful for developers, but for users it is recommended to
#' use [vecvec()] to create vecvec objects.
#' 
#' @param x An unnamed list of arbitrary vectors. The vectors can be of 
#'   different types and lengths, but they must all be vectors (according to
#'   [vctrs::vec_is]). Adjacent vectors of the same type will be combined 
#'   together into a single vector.
#' @param i A vector of integers specifying the location of each element in `x` 
#'   as if they were combined in order. The values in `i` must be between 1 and
#'   the total number of elements across all vectors in `x`, and can contain 
#'   duplicates. If not provided, it defaults to a sequence from 1 to the total 
#'   number of elements across all vectors in `x`.
#' 
#' @return A vector of vectors of S7 object of class `class_vecvec`.
#'
#' @examples
#' # Create a vecvec prototype
#' class_vecvec()
#'
#' # Construct a vecvec from a list of vectors
#' class_vecvec(list(letters, rnorm(10)))
#'
#' # Fully specify a vecvec with locations
#' class_vecvec(
#'   x = list(letters, rnorm(10)),
#'   i = c(1:3, 27:31, 26:4, 32:36)
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
    x <- vecvec_flatten_adj(x)
    i
    new_object(S7_object(), x = x, i = i)
  },
  validator = function(self) {
    vec <- vapply(self@x, vec_is, logical(1L))
    if (!all(vec)) {
      "@x must be a list of primitive vectors"
    }
  }
)

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
