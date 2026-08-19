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
