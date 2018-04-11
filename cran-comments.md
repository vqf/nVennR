## Test environments
* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Reverse dependencies

No reverse dependencies.

---

## Resubmission

This is a major revision with new functions. In this version I have:

* Changed the version number. 
  
* Added multiple functions, all documented and tested.

* Added a vignette to explain the new workflow. Since functions in this packages perform complex simulations, the vignette takes about 20 seconds to compile on my computer. I have tried to minimize compilation time and file size.
