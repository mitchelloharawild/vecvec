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
* mixtime: needs the update submitted before this version (0.5.1), which
  works with this and earlier versions of vecvec. mixtime 0.5.0 relies on
  how vecvec's vctrs methods worked before. I maintain mixtime.
