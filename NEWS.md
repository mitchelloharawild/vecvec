# vecvec 2.0.0

## Improvements

* The ALTREP base vector of vecvec 1.5.0 is gone. A `vecvec` whose elements
  all share one type is now stored as the data of that type: a single-type
  `vecvec` of numbers, strings or dates is an ordinary vector of those values,
  with the type's attributes kept in a zero-length prototype. Base R and C code
  that reads a `vecvec` directly sees its values in their own type (not
  converted to doubles), so `lm()`, `glm()`, `model.matrix()`, `prcomp()` and
  `data.matrix()` work on single-type `vecvec` columns as they do on the plain
  vectors.

* A `vecvec` of mixed types has the integer index into its stored vectors as
  its data again (as in vecvec 1.4.0), and modelling functions (anything using
  `model.frame()`) now error for it rather than fitting on its numeric readings
  (1.5.0) or the index (1.4.0). They also error for a `vecvec` of
  factors, since models treat factors by their class, which a `vecvec` doesn't
  have; convert it with `unvecvec()` first.

* vctrs now treats a single-type `vecvec` as a vector of its type, and a
  mixed-type `vecvec` as a vector of any types (see Breaking changes). vctrs
  operations on a single-type `vecvec` (`vec_slice()`, `vec_c()`,
  `vec_match()`, joins, ...) are now about as fast as on the plain vector, and
  those on mixed-type `vecvec`s are also much faster.

* New `vecvec_cast()` casts a `vecvec` to the vctrs type of another. A
  single-type `vecvec` cast to a mixed-type `vecvec` is still stored as its
  values, so models and `is.numeric()` see them.

* A mixed-type `vecvec` whose types have no common type can now be sorted
  (`sort()`, `order()`, `vec_order()`, ...): by type, in the order the types
  first appear, then by value. Types with a common type (e.g. integers and
  doubles) still sort by value.

* `cumsum()`, `cumprod()`, `cummax()` and `cummin()` now work on single-type
  `vecvec`s, as on the plain vector.

* Subsetting, arithmetic, math functions, `c()`, `rep()`, `is.na()`,
  `duplicated()`, `unique()`, `match()` and `%in%` of single-type `vecvec`s are
  now about as fast as on the plain vector. Operations on mixed-type `vecvec`s
  are also faster, especially arithmetic and comparisons with a single value
  (e.g. `x + 1`, `x == "a"`) or with another mixed-type `vecvec`.

* `is.numeric()` is now `TRUE` only for a single-type `vecvec` whose type is
  numeric (so not for mixed numeric types), following the contract of
  `is.numeric()` methods in base R.

* Everything a `vecvec` needs is in its data and attributes, so code that
  copies a vector's data and attributes in C (data.table, tidyr, reshape2,
  collapse, serialization) produces a correct `vecvec`, whatever its types.

* Vectors of the same type are now always stored together, not only when
  adjacent, so a `vecvec` is single-type exactly when all of its elements share
  a type (including attributes, e.g. a factor's levels). Missing values without
  a type (logical `NA`) don't count as another type, so `c(x, NA)` and
  `x[i] <- NA` keep a single-type `vecvec` single-type.

## Breaking changes

* As for any other vctrs type, casting to a single-type `vecvec` casts the
  values to its type, and the common type of two `vecvec`s is single-type only
  when both have that type. So `vec_assign()` (and functions using it, like
  `tidyr::replace_na()`) can't put values of another type into a single-type
  `vecvec`; base `[<-` still can. `vec_ptype()` of a single-type `vecvec` keeps
  its type.

* This applies to subclasses of `class_vecvec` too. A subclass's empty
  prototype meaning "any vector of the subclass" must hold no types (an empty
  vector holding a type is a vector of that type), and a `vec_cast()` method
  that a subclass registers itself between two of its vectors must return the
  result of `vecvec_cast()`. mixtime >= 0.5.1 does this.

* `class()` of a `vecvec` no longer includes `"integer"` (or `"double"`), and
  `typeof()` is the type of its values (single-type) or `"integer"` (mixed).

* Packages that extend `class_vecvec` must be reinstalled after updating
  vecvec, as S7 copies a parent class into the packages that extend it when
  they're installed. Loading such a package errors with a message saying which
  package to reinstall.

* Single-type `vecvec`s no longer share storage between repeated elements
  (e.g. after `rep()`), as their data is their values. Mixed-type `vecvec`s
  still do.

## Bug fixes

* vctrs equality now compares the values of different `vecvec`s, so
  `vec_match()`, `vec_in()`, `vec_equal()` and joins no longer give wrong
  results (or errors) when the `vecvec`s hold different types, or the same
  types in a different order.

* `vec_detect_missing()` now detects missing values in a `vecvec` (so
  `dplyr::coalesce()`, `vec_fill_missing()` and friends work). All missing
  elements are equal to one another, whatever their type.

* `vec_detect_complete()` of a mixed-type `vecvec` now flags only its missing
  elements, rather than every element. This fixes `vec_rank(incomplete =
  "na")`, `dplyr::min_rank()` and friends (which gave all `NA`) and
  `tidyr::drop_na()` (which dropped every row).

* `vec_compare()` (and so `vec_rank()`) now compares the values of two
  `vecvec`s, rather than their ranks within each.

* `match()` and `%in%` now compare single-type `vecvec`s by their values, as
  for the plain vector, and mixed-type `vecvec`s as the common type of their
  types (e.g. integers and doubles as doubles), as for the plain vectors
  combined. They were compared as strings, so e.g. `0.1 + 0.2` matched `0.3`.
  Mixed types with no common type are still compared as strings, as
  `match()` does for plain vectors.

* Values of a type whose prototype holds functions (such as an S7 class) now
  combine with the same type read back from a file (e.g. with `readRDS()`),
  so the result stays single-type rather than holding the type twice.

* `[<-` now ignores `NA` positions in a numeric index when the value has size
  1, as base R does (a longer value still errors).

* `[<-` past the end of a `vecvec` now extends it with missing elements, rather
  than returning an invalid object (or, for mixed types, pointing the gap at
  stored values).

* `[<-` with a `vecvec` of missing elements no longer leaves index entries
  pointing past the stored values ("subscript out of bounds" afterwards).

# vecvec 1.5.0

## Improvements

* The base vector of a `vecvec` is now an ALTREP double vector that resolves to
  the stored values, instead of an integer index. Base R and C code that reads a
  `vecvec` directly, such as `lm()`, `data.matrix()` and `prcomp()` on data
  frames with `vecvec` columns, now uses the values rather than the indices.
  Stored values with no numeric reading (e.g. character) resolve to `NA`.
  Classed numeric vectors (e.g. cyclical time) resolve through their
  `as.double()` method, so the base agrees with `as.numeric()`.
  `is.integer()` is now `FALSE`.

* `identical()` now tells `vecvec`s with different values or indices apart. The
  stored vectors and index are mirrored in a `_vecvec` attribute (the same
  object as the ALTREP data, so it uses no extra memory and is serialized once),
  as `identical()` can't see ALTREP data and the base resolves non-numeric slots
  to `NA`. Previously it was `TRUE` for e.g. `vecvec("a")` and `vecvec("b")`.

* `vecvec_mapply()` and `Ops` now call `.f` / the operator once per distinct
  combination of underlying slots, rather than once per contiguous run of
  positions. Interleaved vectors (e.g. from `x[order(...)]`) no longer make one
  call per element, and `Ops` is no longer quadratic for them. `.f` receives
  every position that shares a slot combination (in position order), so it must
  be vectorised and element-wise, as documented.

* Merging adjacent same-type slots (in `class_vecvec()`, `c()`, etc.) is now
  linear in the number of slots rather than quadratic.

* Updates for compatibility with S7 > 0.2.2.

## Bug fixes

* Slots that are matrices or data frames are now measured and subset by row
  (with `vec_size()` and `vec_slice()`), instead of by cell or column with
  `length()` and `[`. This fixes subsetting, replacement, `c()` and
  `vecvec_apply()` for `vecvec`s containing these vector types.
* `[<-` on an array `vecvec` (e.g. `x[i, j] <- value`) no longer drops the
  `dimnames`.

# vecvec 1.4.0

## Improvements

* Restoring a `vecvec` from its vctrs proxy is much faster (no longer quadratic
  in its length). Rows are now grouped by the identity of their slots rather 
  than hashing their contents once per row.
* `vecvec_apply()` now splices `vecvec` results from `.f` into the result
  instead of nesting them, so `unvecvec()` simplifies them directly.
  
## Bug fixes

* `vec_c()` and `vec_rbind()` (and so `dplyr::bind_rows()`) no longer
  reorder elements when the inputs' storage is interleaved, e.g.
  `vec_c(a, b, a)` previously returned the elements of `b` last.
* `c()` of a single zero-length `vecvec` (e.g. `c(x[0])`) no longer errors
  with "Underlying data must be <integer> not <list>".
* `duplicated()`, `anyDuplicated()` and `unique()` now detect duplicates
  among elements whose indices point at the same stored value, e.g. after
  `x[c(1, 1)]`. Missing indices are now duplicates of each other rather than
  `NA`.

# vecvec 1.3.0

## New features

* Added `vecvec_mapply()`, which applies a vectorised function across
  multiple (possibly heterogeneously-typed) vectors, batching calls by
  shared underlying storage slots rather than calling it once per element
  as `mapply()` does.

## Improvements

* Reworked formatting of array vecvecs to prevent ALTREP materialisation.
* `duplicated()`/`anyDuplicated()`, `vec_proxy_equal()`, and casting into a
  `vecvec` are now computed slot-wise instead of materialising every element.
* ALTREP detection (used to avoid materialisation when merging adjacent slots)
  now uses a C-level check rather than parsing `.Internal(inspect())`.
* Added support for casting into a `vecvec` with duplicated indices.
* Errors, warnings, and messages now use the `cli` package.

## Bug fixes

* `[.vecvec` now compacts slots to actually referenced rows.
* Added `is.numeric()` method for `vecvec`, which checks the type of its
  slots rather than the container itself.
* Added `all.equal()` method for `vecvec`. Previously the default method
  compared the underlying storage (indices and numeric-only slot content)
  rather than the represented values, giving misleading results or erroring
  whenever a slot held a non-numeric type.
* Fixed `as.data.frame()` on a `vecvec` always erroring. It now wraps the
  `vecvec` as a single column, as is done with atomic vectors.
* Fixed `[<-` and `is.na<-` corrupting compressed storage shared by other,
  unreplaced elements when only some of the sharers were overwritten.
* Fixed `duplicated()`/`anyDuplicated()` incorrectly destructuring `vctrs`
  record-style slots (e.g. `vctrs_rcrd`) element-by-element.
* Fixed cumulative `Math` generics (`cumsum()`, `cumprod()`, `cummax()`,
  `cummin()`) silently erroring.
* Fixed `unvecvec()` scattering values with single-bracket indexing, which is
  column (not row) selection for matrix and data frame results. This silently
  gave wrong answers whenever a slot held more than one distinct row, and
  hard-errored whenever an `NA` index was present. It now uses
  `vctrs::vec_slice()`/`vctrs::vec_assign()`, which respect rows.

# vecvec 1.2.0

## Improvements

* Allow registration of custom `vctrs::vec_ptype2` and `vctrs::vec_cast` 
  methods.
* Better printing of empty vecvec objects.

# vecvec 1.1.0

## Improvements

* `print()` method now respects the `max.print` option and prevents ALTREP materialisation.
* ALTREP vectors are no longer flattened on creation of vecvec objects.

## Bug fixes

* Fixed `[<-` value replacement with NA indices.
* Fixed `[<-` incorrectly mutating a `vecvec` when the index selects no elements (e.g. `x[FALSE] <- NA`). The object is now returned unchanged.

# vecvec 1.0.0

The `vecvec` class has been reimplemented using S7, replacing the previous vctrs
implementation. Existing code using `vecvec` objects should continue to work.

## Breaking changes

* Removed `new_vecvec()`, which is replaced by `class_vecvec` for S7 consistency.
* Extension packages building new vecvec data types will need to use S7 classes
  that have the parent class `class_vecvec`.
* The internal data structure for `vecvec` objects has been overhauled. In
  practice these internal indices should not be used by users or developers,
  as the structure can change in the future to accommodate faster variants for
  special cases (#7).

## New features

* `vecvec` classed objects now work as matrices and arrays (#15).
* `is_vecvec()` tests whether an object is a `vecvec`.
* `class_vecvec` S7 class and constructor for vecvec objects.
* Added `vctrs` methods for backwards compatibility and vctrs interoperability.

## Improvements

* Substantially faster performance and reduced object size
* Adjacent compatible vectors are now automatically flattened when constructing
  or modifying a `vecvec`, reducing fragmentation.

# vecvec 0.2.1

## Bug fixes

* Fixed missing value handling with `is.na()` for `vecvec` objects (#8).

# vecvec 0.2.0

## New features

* Added `vec_math()` for `vecvec` objects.
* Added `vec_apply()` for applying functions to each vector in a `vecvec`.

## Improvements

* vecvec now works with `vctrs::new_rcrd` type vectors.
* Added `class` argument `vecvec()` to specify subclasses.
* Removed unnecessary cast in `Ops` with `vecvec` types.

## Bug fixes

* Fixed error when `Ops` when `vecvec` and second argument are both length 1.
* Fix casting from `vecvec` dropping common attributes.
* Fixed `vec_proxy_equal()` not comparing values across vectors.

# vecvec 0.1.0

## New features

* `new_vecvec()` and `vecvec()` class constructors.
* `unvecvec()` class deconstructor to reduce back to atomic vectors.
* `{vctrs}` method dispatch for `vecvec` data types.
