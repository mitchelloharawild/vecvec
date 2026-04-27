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
