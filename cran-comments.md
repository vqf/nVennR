## Test environments
* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

No reverse dependencies.

---

Version 0.2.1. No major changes. Some non-essential dependencies caused problems in certain platforms, therefore:


* I have removed the code referencing RcppProgress.

* I have moved rsvg and grImport2 to the Suggests sections. If they are installed, the package should work as before. If they are not installed, there is no output to the plot window, but users can still export the result.

* I have run R CMD check with devtools::check(cleanup = FALSE, args = c('--as-cran')) .
  
Like in the previous version, the vignette is large (about 3.7 Mb).

