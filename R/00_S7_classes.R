#' S7 class for vecvec
#'
#' `class_vecvec()` constructs a new vecvec object from a list of vectors. It is
#' designed to be performant with minimal checks on the inputs. Direct use of
#' `class_vecvec()` is useful for developers, but for users it is recommended to
#' use [vecvec()] to create vecvec objects.
#'
#' @details
#' A vecvec is stored in one of two forms, chosen automatically from its
#' contents:
#'
#' * **single-type**: every element has the same type, held in an atomic
#'   vector (numbers, strings, dates, ...). The vecvec's data *is* that vector's
#'   data, so base R code that reads it directly (such as `lm()` or
#'   `model.matrix()`) sees its values.
#' * **mixed-type**: anything else. The vecvec's data is an integer index into
#'   the stored vectors. Modelling functions error for mixed-type vecvecs rather
#'   than fit on the index.
#'
#' Vectors of the same type (with identical attributes) are always combined, so
#' a vecvec is single-type exactly when all of its elements share one type.
#'
#' vctrs treats a single-type vecvec as a vector of its type, and a mixed-type
#' vecvec as a vector of any types. So, as for any other vctrs type:
#'
#' * Combining vecvecs (e.g. with [vctrs::vec_c()] or `dplyr::bind_rows()`)
#'   gives a single-type vecvec when they all have the same type, and a
#'   mixed-type vecvec otherwise.
#' * Casting to a single-type vecvec casts the values to its type, so
#'   [vctrs::vec_assign()] (and functions using it, like `tidyr::replace_na()`)
#'   can't put values of another type into a single-type vecvec. Base `[<-`
#'   can, and gives a mixed-type vecvec.
#'
#' Subclasses opt in to this with `vecvec_register(single_type = TRUE)`;
#' otherwise all vecvecs of a subclass are one vctrs type (see
#' [vecvec_register()]).
#'
#' @param x An unnamed list of arbitrary vectors. The vectors can be of
#'   different types and lengths, but they must all be vectors (according to
#'   [vctrs::vec_is]). Vectors of the same type are combined together into a
#'   single vector.
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
  # The data of a vecvec is either the values of its single type or an integer
  # index (see R/layout.R), so it can't have a fixed base type. An S3 parent
  # without classes allows any base type without adding to `class()`.
  parent = new_S3_class(character(), constructor = function(.data = integer()) .data),
  # Packages that extend this class keep a copy of it from when they were
  # installed (S7 classes are stored by value), and these functions are run
  # from those copies. So they only call the stable entry points in
  # R/layout.R, which must keep their names and arguments.
  properties = list(
    x = new_property(
      class_list,
      # The stored vectors are derived from the data, and replacing them keeps
      # the index.
      getter = function(self) vv_slots(self),
      setter = function(self, value) vv_set_slots(self, value)
    )
  ),
  constructor = function(x = list(), i = seq_len(sum(list_sizes(x)))) {
    new_object(vv_new_data(x, i))
  },
  validator = function(self) vv_validate(self)
)
