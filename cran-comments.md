The checks fail on win-builder R-devel, but pass on Linux and Windows with 
r-devel r90655. It is believed that this is a win-builder specific issue.

## Test environments
* local ubuntu 24.04 install, R 4.5.2 and R-devel (4.7.0, r90655)
* ubuntu-latest (on GitHub actions), R-devel, R-release, R-oldrel-1, R-oldrel-2, R-oldrel-3, R-oldrel-4
* macOS-latest (on GitHub actions), R-release
* windows-latest (on GitHub actions), R-release, R-oldrel-4
* win-builder, R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

Reverse dependency checks have been performed and there were no changes to worse.
