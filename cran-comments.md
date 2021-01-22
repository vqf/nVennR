## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub linux-x86_64-rocker-gcc-san (r-devel)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

* This is a new release.

## Reverse dependencies

No reverse dependencies.

---

Version 0.2.3. No major changes:

* Previous versions of the vignette used Cairo svg(), as the output of the plotting functions is svg code. In this version, I have implemented hooks to show the resulting svg code directly in the html result. As a result, the vignette is not as large as before (327 Kb).

* I have run R CMD check with `devtools::check(cleanup = FALSE, args = c('--as-cran'))`.
  
* I have run tests on rhub with rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_`="false")). 

* I have added a `Readme` with a badge for tests in `travis-ci.com`.

## Expected NOTEs and ERRORs

* DESCRIPTION NOTE: rhub says that the DESCRIPTION DOI return a HTTP 403 error (forbidden). The page [https://doi.org/10.1093/bioinformatics/bty109](https://doi.org/10.1093/bioinformatics/bty109) works in browsers in private mode without javascript.
