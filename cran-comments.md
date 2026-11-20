## Resubmission

This replaces the pending submission of vecvec 1.5.0 (submitted 2026-11-18).
That version made the base of a `vecvec` an ALTREP vector; this one stores a
`vecvec` of a single type as an ordinary vector of its values instead, and
drops ALTREP entirely.

## Test environments
* local ubuntu 24.04 install, R 4.6.2 and R-devel (r90655)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

We checked the 2 reverse dependencies, comparing R CMD check results across
CRAN and dev versions of this package.

* ggtime: no changes.
* mixtime: one test fails, which checks that `lm()` and `prcomp()` read
  mixtime's cyclical times as `as.numeric()` does. With this version they read
  the times' own data, as they do for a plain cyclical time vector. I maintain
  mixtime and will submit an update with the corrected test as soon as this
  version is accepted.
