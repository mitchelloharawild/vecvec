# vecvec (development version)

## New features

* Added `vec_math()` for `vecvec` objects.

## Improvements

* vecvec now works with `vctrs::new_rcrd` type vectors.
* Added `class` argument `vecvec()` to specify subclasses.
* Removed unnecessary cast in `Ops` with `vecvec` types.

## Bug fixes

* Fixed error when `Ops` when `vecvec` and second argument are both length 1.
* Fix casting from `vecvec` dropping common attributes.

# vecvec 0.1.0

## New features

* `new_vecvec()` and `vecvec()` class constructors.
* `unvecvec()` class deconstructor to reduce back to atomic vectors.
* `{vctrs}` method dispatch for `vecvec` data types.
