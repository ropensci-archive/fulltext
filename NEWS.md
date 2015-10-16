fulltext 0.1.4
===============

### NEW FEATURES

* New function `ft_get_si()` to grab supplementary files for any
article (#61) (#62) - thanks @willpearse
* New function `ft_links()` to grab links for the full text version 
of an article from entire output of `ft_search()`, their individual 
components (i.e., data sources), or from character vector of DOIs (#36)

### MINOR IMPROVEMENTS

* Lowercased all column headers from `ft_search()` (#63)
* DRYed out plugins for `ft_get()` to reduce code duplication (#48)

### BUG FIXES

* Fixed bug in `ft_search()` where limit integer was too big (#57)
* Fix to `ft_get()` to create a directory if it doesn't exist 
already (#56)
* Fixed bug in `ft_search()` that caused problems in some scenarios (#55)
* Better error message when pdf passed to `pdfx()` function when 
pdf is not useable, that is, e.g., a scanned pdf (#53)

fulltext 0.1.0
===============

* Released to CRAN.
