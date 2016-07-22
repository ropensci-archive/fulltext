fulltext 0.1.8
===============

### MINOR IMPROVEMENTS

* require newest `rcrossref` and `rplos` versions that use `dplyr::bind_rows()` instead
of `dplyr::rbind_all()` to avoid errors/warnings (#89) (#90)

fulltext 0.1.6
===============

### MINOR IMPROVEMENTS

* More documentation added for the `from` parameter for `ft_get_si()` to clarify its use, and fails better when used inappropriately (#68) (#77)
* `ft_get_si()` now gives file type information as attributes so that downstream uses can access that information instead of having to guess file types (#69)

### BUG FIXES

* Fixes to `ft_get_si()` to work with changes in the publisher Wiley's URL changes (#71) (#73)

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
