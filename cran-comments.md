
## v0.4.5

Date: 20200408
1. Use `\donnttest` instead of `\donntrun`.
2. Remove `rm(list = ls())` in examples.
3. Check every function to make sure they have a returned value explanation

Date: 20200404
0. Fix the CRAN comments, remove `\donntrun` and use tempfile for the example in `parse_fst`.
Intro:A mirror package of tidyfst, using modification by reference whenever possible. This toolkit is designed for users who want absolute speed with least memory.


## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
