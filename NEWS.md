fulltext 2.0
==============

### MINOR IMPROVEMENTS

* `ft_abstract()`: change default `from` value from "plos" to "crossref" (#235)

### BUG FIXES

* fix PLOS internals for `ft_abstract()` (#234)
* fix how Miscrosoft Academic query was structured before being sent off - affecting `ft_search()` (#236)


fulltext 1.7.0
==============

### NEW FEATURES

* The Cross Text and Data Mining program has ended. It ended officially at the end of 2020, but it took Wiley until early February to implement a TDM service. This impacts `ft_get` only, and impacts retrieval of only Wiley and Elsevier works (which is a lot since they're very large publishers). You now have to get a TDM key from both Wiley and Elsevier. See the "Authentication" section in `?fulltext` for details. See also `?"ft_get-warnings"`: there's a new warning if we detect that you have a `CROSSREF_TDM` env var   (#224)
* Added new `ft_get` plugin for Trans Tech Publications (#232)
* Import dependency `crminer` dropped. A few `crminer` functions brought over into this package and renamed. This is due to Crossref TDM program ending (#233)

### MINOR IMPROVEMENTS

* new `ft_search()` docs section "Pagination" describing what parameter names to use for each data source to do pagination

### BUG FIXES

* `ft_search()` fix: some data source specific options (e.g., `plosopts`) were being overridden by the global equivalent options (e.g., in `ft_search(limit = 5, from = c("plos", "entrez"), plosopts = list(limit=10))` the limit sent to PLOS was 5 instead of 10). Fixed now so that data source specfiic options override the matching global option (#229)
* Internal API Fatcat changed its parameter name to limit what fields are returned. Fixed now (#231)


fulltext 1.6.0
==============

### NEW FEATURES

* HTTP requests previously made to the ftdoi.org API are now done locally with data/files cached locally; speeds up `ft_get` usage, especially if you're on a slow internet connection (#225) (#226)

### MINOR IMPROVEMENTS

* all vignettes only available on docs site now: https://docs.ropensci.org/fulltext/ (#227)

### BUG FIXES

* `ft_links()` opts parameters dropped (e.g., `plosopts`); no options can be passed down to each data source (#223)


fulltext 1.5.0
==============

### NEW FEATURES

* `ft_abstract()` gains new data source option: Semantic Scholar. Uses async http requests. Beware of rate limits. See 'Rate limits' in `?fulltext-package` (#218)

### MINOR IMPROVEMENTS

* using vcr for some tests (#212)
* improved documentation on proxy settings for Scopus data source added to the 'Authentication' section of the package level manual file, see `?fulltext-package` (#220) thanks @behrica
* for `ft_get()` added new plugin helper for publisher Company of Biologists; and a fix in the Company of Biologists metadata returned from the ftdoi.org API (#216)
* cleaned out old code for brief data.frame printing, use tibble instead

### BUG FIXES

* `from` parameter in `ft_get()` for specifying what data source to use should have been accepting more than 1 value, but was only accepting 1 value. Now `from` accepts 1 or more values (#222)


fulltext 1.4.0
==============

### NEW FEATURES

* `ft_get()` gains a new publisher plugin: Cambridge. its not available in the `from` parameter, but if you pass in a Cambridge DOI, a plugin now exists to attempt to get the pdf (xml not provided) (#195)

### MINOR IMPROVEMENTS

* Wiley now provides xml (in addition to pdf) for at least some articles. So `ft_get()` for Wiley now supports `type="xml"` in addition to `type="pdf"` (#209)
* reduce duplicated code in `ft_get` plugins for checking `type` parameter (#210)
* changed Wiley plugin for `ft_get()`: using different URLs for attempting to fetch articles, those with base url `api.wiley.com` instead of `onlinelibrary.wiley.com` (#191)

### BUG FIXES

* `ft_get()` wasn't allowing `type="pdf"` for PLOS data source; now allowed in addition to xml; also plos requests for articles now using http instead of http (#205) thanks @clbti for the report
* fix to internal fxn `fat_cat_search_one()` used inside of `ft_get()`: fixed data.frame subsetting error due to sometimes missing columns (#206)
* fix to `ft_links()`: was erroring when publisher not supported yet; now gives back no data (#207)
* `ft_browse()` was failing when `what="macrodocs"` selected; macrodocs.org is now dead; `what` parameter now defunct  (#208)
* fix `ft_table()` - was failing on reading malformed files (e.g., an xml file with pdf content inside)  (#211)


fulltext 1.3.0
==============

### NEW FEATURES

* `ft_get()` gains new data source: ScienceDirect (#196) thanks @knh11545
* performance improvement to `ft_get()`: when using internal fxn `get_unknown`(when publisher is not initially knowsn) we run another internal fxn `get_publisher` which is slow cause can only throw one DOI per request to Crossref API; We now use the FatCat (https://fatcat.wiki/) API to do more efficient publisher lookup (#192) (#201)

### MINOR IMPROVEMENTS

* curl options can be passed into most functions now (#199)
* change to `ft_get()` when using Elsevier: we now by default detect if the downloaded file is abstact only and treat that as you dont have access - you can toggle this within `elsevieropts` - see `?ft_get` for help (#200)
* fix link for fulltext manual, ropensci->ropenscilabs (#204)

### BUG FIXES

* fix to `ft_abstract()`: when using plos, we were passing on identifiers to the wrong Solr parameter passed to PLOS Solr API (#202) thanks @mrweiler !
* fix to `ft_links()` with data from Entrez was failing when NULLs were encountered (#203)


fulltext 1.2.0
==============

### NEW FEATURES

* `ft_get()` gains a `progress` parameter, `TRUE` printing a progress bar and `FALSE` not. By default we do not print a progress bar to be consistent with the behavior of previous versions (#140) (#190)
* `cache_options_set()` gains new parameter `full_path` to set the entire path to the cache, use like `cache_options_set(full_path = yourpath)` (#185) thanks @bomeara for the feature request

### DEFUNCT

* `ft_chunks()` and `ft_tabularize()` were deprecated in the previous version, and are now defunct. See the new package https://github.com/ropensci/pubchunks for the same (and improved) functionality (#146) (#181)
* `ft_get_si()` is defunct. It's been pulled out into a new package suppdata  (#186) (#188)

### BUG FIXES

* Fix to `eupmc_search()` internal function. At some piont Europe PMC changed the way they do paging, what parameters are used, etc. Fixed now. See examples for how to do paging; uses a cursor model instead of a rows/offset model  (#184) thanks @jshleap for the report
* two hopefully fixes for Wiley URLs: a) try a 2nd url pattern if the first fails in general for Wiley links; and b) if highwire links found, replace with a url pattern that should work (#189) thanks @bomeara for the report

### MINOR IMPROVEMENTS

* remaining `httr` code removed, now using `crul` for all HTTP requests (#187)
* filled out `Scopus` and `Crossref TDM` parts in the Authentication section of the package level manual file `?fulltext-package`; and add more details on authentication in the `?ft_get` manual file  (#182) (#183)


fulltext 1.1.0
==============

### NEW FEATURES

* gains new function `cache_file_info()` to get information on possibly bad files in your cache - which you can use to remove files as you see fit (#142) (#174) thx @lucymerobinson for the push
* gains new function `as.ft_data()` to create the same output as `ft_get()` returns, but instead pulls all files from your cache (#142) (#172) thanks @lucymerobinson
* `ft_get()` gains new attribute of a data.frame in the `errors` slot with information on each article and what error we collected or `NA_character_` if none; should help with sorting out problems across all requests (#176)
* scopus option in `ft_search()` gains support for facets; see `?scopus_search` (#170) thanks @lucymerobinson

### BUG FIXES

* fixed bug in `ft_search()` for microsoft academic plugin (#154)
* fixed bug in `ft_search()` for scopus plugin - we weren't looping over requests correctly (#161)
* fix bug in `ft_links()` - when result of `ft_search()` passed to `ft_links` with bad urls or with more than 1 then `ft_links` was failing; fix by filtering on `intended-application` field from Crossref via fix in dependency package `crminer` (#173)
* additional check added in `ft_get()` to check for an invalid file that gave a 200 status code (so passes the status code check) (#175)
* bring back support for `start` parameter in `ft_search()` for Scopus to offset what record a query starts at (#180)
* fix to `ft_get()` for entrez data source to loop internally when more than 50 ids requested to avoid 414 http error (URI too long) (#167) thanks @t088

### MINOR IMPROVEMENTS

* change base url for EuropePMC to `https`
* use `https` for all `doi.org` requests (#155) thanks @katrinleinweber 
* better explanation of the may `opts` parameters in `ft_search()` (#161)
* after many attempts, finally (I think) sorted out Microbiology Society for the `ft_get()` function (#163) thanks to @low-decarie
* mention `suppdata` package in the README (#164)
* clarify language in `ft_collect()` that we are saving files to disk locally only (#165)
* fix typos (#166) (#168) thanks @maelle @AugustT
* removed `whisker` package dependency (#156)
* scopus gains support for more parameters; see `?scopus_search`  (#152)
* updated docs with information on using NCBI Entrez API keys (#159)
* plugin for publisher Instituto de Investigaciones Filologicas to `ft_get()` added (#117) thanks @andreifoldes 
* clarified in `ft_search()` docs that for some sources we loop internally to get whatever number of records the user wants, while others we can not (#162)

### DEPRECATED

Continuing to focus the scope of this package the functions `ft_chunks()` and `ft_tabularize()` are now deprecated, and will be removed (defunct) in a future version of this package. See the new package pubchunks for the same and better functionality. (#181)


fulltext 1.0.1
==============

### BUG FIXES

* Fix bug in internal function `get_ext()` which parses either xml, pdf, or plain text from files on disk - it was failing on Linux maxchines due to a faulty regex (#151)

### MINOR IMPROVEMENTS

* Updated formats vignette, three publishers that used to provide XML no longer do (#150)


fulltext 1.0
============

Check out the fulltext manual (https://books.ropensci.org/fulltext/) for detailed documentation.

`fulltext` has undergone a re-organization, which includes a bump in the major version to `v1` to reinforce the large changes the package has undergone. Changes include:

- Function name standardization with the `ft_` prefix. e,g, `chunks` is now `ft_chunks`
- `ft_get` has undergone major re-organization - biggest of which may be that all full text XML/plain text/PDF goes to disk to simplify the user interface.
- `storr` is now imported to manage mapping between real DOIs and file paths that include normalized DOIs - and aids in the function `ft_table()` for creating a data.frame of text results
- Note that with the `ft_get()` overhaul, the only option is to write to disk. Before we attempted to provide many different options for saving XML and PDF data, but it was too complicated. This has implications for using the output of `ft_get()` - the output is only the paths to the files - use `ft_collect()`  to collect the text if you want to use `ft_chunks()` or other `fulltext` functions downstream.

### NEW FEATURES

* `chunks` changed to `ft_chunks` (#139)
* `collect` changed to `ft_collect` (#139)
* `tabularize` changed to `ft_tabularize` (#139)
* `get_text` changed to `ft_text` (#139)
* `ft_get()` gains new parameter `try_unknown` that attempts to try to find full text for a given DOI/ID even if we don't have code plugins specifically for that publisher. This includes trying to get a full text link from Crossref and the ftdoi.org API (#137)
* Gains function `ft_table` that outputs a data.frame of all downloaded articles with DOIs, filenames, and the text of each article, similar to the `readtext` package (#134)
* Gains function `ft_abstract` for fetching abstracts, including support for getting abstracts from Scopus, Microsoft Academic, Crossref, and PLOS (#98) (#115)
* Microsoft Academic added as another data source both in `ft_abstract` and in `ft_search` via the `microdemic` package (#99) (#115)
* `ft_get()` gains an S3 method for `ft_links` - that is, you can pass the output of `ft_links()` to `ft_get()` (#103)
* `ft_get()` gains many new plugins, including for: Informa, Scientific Societies, Europe PMC, Elsevier, Wiley, xxx (#121) (#112) (#52) (#96) (#120) (#xxx)
* Gains new functions to list available plugins for each of `ft_get()`/`ft_links()`/`ft_search()`: `ft_get_ls()`/`ft_links_ls()`/`ft_search_ls()` (#122)
* `ft_chunks()` gains support for Elsevier XML (#116) (#118)
* Scopus added as a new datasource in both `ft_search()` and `ft_abstract` (#95)
* gains new object `ftxt_cache` for managing/listing details of cached files from `ft_get()`
* `ft_search()` gains Scopus and Microsoft Academic options
* `ft_serialize()` loses `file`, `rcache`, and `redis` options, retaining options for converting between list, JSON, and XML formats.
* The package level manual file at `?fulltext-package` gains new sections on authentication and rate limiting
* gains new manual file for how to interpret warnings when using `ft_get()`; see `?ft_get-warnings`

### DEFUNCT

With the re-focusing of the package these functions seemed out of scope, so have been removed:

* `pdfx`, `pdfx_html`, and `pdfx_targz` are now defunct (#145)
* `ft_extract_corpus` is now defunct

The following functions have changed names. Usually I'd mark functions as deprecated in a version, then defunct in the next version, but since we're moving to `v1` here, it made sense to rip the bandade off and make the old function names defunct.

* `chunks` is now defunct - function name changed to `ft_chunks`
* `tabularize` is now defunct - function name changed to `ft_tabularize`
* `collect` is now defunct - function name changed to `ft_collect`
* `get_text` is now defunct - function name changed to `ft_text`

Other defunct:

* `cache_clear` was never working anyway, and is now removed.

Along with the above changes and others the packages `R.cache`, `rredis`, `digest`, and `tm` have been removed from Imports

### MINOR IMPROVEMENTS

* Replaced `httr` with `crul` mostly throughtout the package (#104)
* We are now using DOIs in file names written to disk from `ft_get()`. DOIs are normalized before using to create file paths (#138)
* Output of `ft_get()` should now be correctly named lists after the publisher and the DOI/ID (#126)
* Switched for eLife DOIs to attempt to get full text links from Crossref API instead of constructing by hand (#127)
* Now using `hoardr` package for managing cached files (#124)
* sending user agent string for this R pkg to the ftdoi.org API when calling it now (#141)
* Documented option to include your email address with Crossref API queries to get in their "fast lane" (#123)
* Documented how to get rate limit headers/information for Elsevier/Scopus requests (#109)
* For text extraction from PDFs - only using `pdftools` now - done away with other options (#82)
* `biorxiv_search` is now exported but the man file is hidden (using `roxygen2` directive `@internal`), so you can still get to the manual file doing `?biorxiv_search`

### BUG FIXES

* New internal PLOS API method instead of a function from `rplos` 
because we need to write to disk instead of return parsed XML 
output (#148)
* `ft_get()` now appropriately using cached version of file if found (#130)
* The `type` parameter in `ft_get()` was ignored previously, now definitely
used. (#128)
* Fix `biorxiv_search` (#97) (#113)


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
