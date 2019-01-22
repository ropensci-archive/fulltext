## Test environments

* local OS X install, R 3.5.2 patched
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

License components with restrictions and base license permitting such:
     MIT + file LICENSE
   File 'LICENSE':
     YEAR: 2019
     COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

* I have run R CMD check on the 2 downstream dependencies. No errors were found. 
Summary at <https://github.com/ropensci/fulltext/blob/master/revdep/README.md>
* The revdep maintainers were notified of the release

--------

This version includes new parameters in a few functions, a number of defunct function, some small improvements, and bug fixes.

There ares a three failures on R-devel with _R_CHECK_LENGTH_1_LOGIC2_=TRUE in the package test suite that track down to a problem with the xml2 package `parse_options(options, xml_parse_options())`. I've notified the xml2 maintainer of this problem https://github.com/r-lib/xml2/issues/238

Thanks! 
Scott Chamberlain
