## Test environments
* local ubuntu 24.04 install, R 4.6.2 and R-devel (r90655)

## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 3

  This release comes soon after 1.5.0 because it replaces the ALTREP base
  vector introduced there, which broke code that copies vectors in C (such as
  `tidyr::gather()` in the ggtime package). A `vecvec` of a single type is now
  stored as an ordinary vector of its values, and ALTREP is no longer used. The
  major version reflects breaking changes to how vctrs treats `vecvec`s.

## Reverse dependencies

We checked the 2 reverse dependencies, comparing R CMD check results across
CRAN and dev versions of this package.

* ggtime: no changes.
* mixtime: needs the update submitted before this version (0.5.1), which
  works with this and earlier versions of vecvec. mixtime 0.5.0 relies on
  how vecvec's vctrs methods worked before. I maintain mixtime.
