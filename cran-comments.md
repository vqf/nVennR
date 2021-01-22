## Test environments
* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

* This is a new release.

## Reverse dependencies

No reverse dependencies.

---

Version 0.2.3. No major changes:

* Previous versions of the vignette used Cairo svg(), as the output of the plotting functions is svg code. In this version, I have implemented hooks to show the result svg directly in the html result. As a result, the vignette is not as large as before (327 Kb).

* I have run R CMD check with devtools::check(cleanup = FALSE, args = c('--as-cran')) .
  
Like in the previous version, the vignette is large (about 3.7 Mb).

