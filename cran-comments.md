## Test environments

* local OS X install, R 3.4.3 patched
* ubuntu 12.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

License components with restrictions and base license permitting such:
     MIT + file LICENSE
   File 'LICENSE':
     YEAR: 2018
     COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

* I have run R CMD check on the 1 downstream dependency. No errors were found. 
Summary at <https://github.com/ropensci/fulltext/blob/master/revdep/README.md>
* The revdep maintainers were notified of the release

--------

This submission overhauls quite a bit of the package, changing a lot of internals, changing function names, and making some functions defunct - thus a bump in major version to v1.0. 

Thanks! 
Scott Chamberlain
