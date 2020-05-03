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

Version 0.2.2. No major changes. rsvg seems to show errors in certain platforms, therefore I have enclosed the relevant parts of showSVG in a tryCatch loop. Other issues:

* The root svg element now has xmlns and xlink options. In some cases, this may help external editors to read the resulting file. Nevertheless, some versions of Adobe Illustrator do not import the resulting svg correctly.

* I have added a showLegend option to showSVG.

* I have run R CMD check with devtools::check(cleanup = FALSE, args = c('--as-cran')) .
  
Like in the previous version, the vignette is large (about 3.7 Mb).

