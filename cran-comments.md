## Test environments
* local OS X install, R 3.4.4
* ubuntu 12.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

## Resubmission

This is a resubmission with a patch. In this version I have:

* Changed the version number. 
  
* Fixed a problem with the persistence of public variables. Repeated execution of toVenn led to slower convergence. Thanks to Frank Ruge for pointing this out.

