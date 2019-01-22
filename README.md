

```
  _____     .__  .__   __                   __
_/ ____\_ __|  | |  |_/  |_  ____ ___  ____/  |_
\   __\  |  \  | |  |\   __\/ __ \\  \/  /\   __\
 |  | |  |  /  |_|  |_|  | \  ___/ >    <  |  |
 |__| |____/|____/____/__|  \___  >__/\_ \ |__|
                                \/      \/
```

[![cran checks](https://cranchecks.info/badges/worst/fulltext)](https://cranchecks.info/pkgs/fulltext)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://api.travis-ci.org/ropensci/fulltext.svg)](https://travis-ci.org/ropensci/fulltext)
<!-- [![Build status](https://ci.appveyor.com/api/projects/status/y487h3ec5wc2s20m/branch/master?svg=true)](https://ci.appveyor.com/project/sckott/fulltext/branch/master) -->
[![codecov.io](https://codecov.io/github/ropensci/fulltext/coverage.svg?branch=master)](https://codecov.io/github/ropensci/fulltext?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/fulltext)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/fulltext)](https://cran.r-project.org/package=fulltext)

__Get full text articles from lots of places__

Checkout the [fulltext manual](https://ropensci.github.io/fulltext-book/) to get started.

-----

rOpenSci has a number of R packages to get either full text, metadata, or both from various publishers. The goal of `fulltext` is to integrate these packages to create a single interface to many data sources.

`fulltext` makes it easy to do text-mining by supporting the following steps:

* Search for articles - `ft_search`
* Fetch articles - `ft_get`
* Get links for full text articles (xml, pdf) - `ft_links`
* Extract text from articles / convert formats - `ft_extract`
* Collect all texts into a data.frame - `ft_table`

Previously supported use cases, extracted out to other packages:

* Collect bits of articles that you actually need - moved to package [pubchunks][]
* Supplementary data from papers has been moved to the [suppdata][] package


It's easy to go from the outputs of `ft_get` to text-mining packages such as 
[tm](https://cran.r-project.org/package=tm) and 
[quanteda](https://cran.r-project.org/package=quanteda).

Data sources in `fulltext` include:

* [Crossref](http://www.crossref.org/) - via the `rcrossref` package
* [Public Library of Science (PLOS)](https://www.plos.org/) - via the `rplos` package
* Biomed Central
* [arXiv](https://arxiv.org) - via the `aRxiv` package
* [bioRxiv](http://biorxiv.org/) - via the `biorxivr` package
* [PMC/Pubmed via Entrez](http://www.ncbi.nlm.nih.gov/) - via the `rentrez` package
* Many more are supported via the above sources (e.g., _Royal Society Open Science_ is
available via Pubmed)
* We __will__ add more, as publishers open up, and as we have time...See the [master list here](https://github.com/ropensci/fulltext/issues/4#issuecomment-52376743)

Authentication: A number of publishers require authentication via API key, and some even more
draconian authentication processes involving checking IP addresses. We are working on supporting
all the various authentication things for different publishers, but of course all the OA content
is already easily available. See the **Authentication** section in `?fulltext-package` after 
loading the package.

We'd love your feedback. Let us know what you think in [the issue tracker](https://github.com/ropensci/fulltext/issues)

Article full text formats by publisher:  [https://github.com/ropensci/fulltext/blob/master/vignettes/formats.Rmd](https://github.com/ropensci/fulltext/blob/master/vignettes/formats.Rmd)


## Installation

Stable version from CRAN


```r
install.packages("fulltext")
```

Development version from GitHub


```r
devtools::install_github("ropensci/fulltext")
```

Load library


```r
library('fulltext')
```

## Search

`ft_search()` - get metadata on a search query.


```r
ft_search(query = 'ecology', from = 'crossref')
#> Query:
#>   [ecology] 
#> Found:
#>   [PLoS: 0; BMC: 0; Crossref: 157839; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0] 
#> Returned:
#>   [PLoS: 0; BMC: 0; Crossref: 10; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0]
```

## Get full text links

`ft_links()` - get links for articles (xml and pdf).


```r
res1 <- ft_search(query = 'biology', from = 'entrez', limit = 5)
ft_links(res1)
#> <fulltext links>
#> [Found] 5 
#> [IDs] ID_30253098 ID_28731711 ID_28097372 ID_27582426 ID_22243231 ...
```

Or pass in DOIs directly


```r
ft_links(res1$entrez$data$doi, from = "entrez")
#> <fulltext links>
#> [Found] 5 
#> [IDs] ID_30253098 ID_28731711 ID_28097372 ID_27582426 ID_22243231 ...
```

## Get full text

`ft_get()` - get full or partial text of articles.


```r
ft_get('10.7717/peerj.228')
#> <fulltext text>
#> [Docs] 1 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.7717/peerj.228 ...
```

## Extract chunks


```r
library(pubchunks)
x <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), from = "elife")
x %>% ft_collect() %>% pub_chunks("publisher") %>% pub_tabularize()
#> $elife
#> $elife$`10.7554/eLife.03032`
#>                          publisher .publisher
#> 1 eLife Sciences Publications, Ltd      elife
#> 
#> $elife$`10.7554/eLife.32763`
#>                          publisher .publisher
#> 1 eLife Sciences Publications, Ltd      elife
```

Get multiple fields at once


```r
x %>% ft_collect() %>% pub_chunks(c("doi","publisher")) %>% pub_tabularize()
#> $elife
#> $elife$`10.7554/eLife.03032`
#>                   doi                        publisher .publisher
#> 1 10.7554/eLife.03032 eLife Sciences Publications, Ltd      elife
#> 
#> $elife$`10.7554/eLife.32763`
#>                   doi                        publisher .publisher
#> 1 10.7554/eLife.32763 eLife Sciences Publications, Ltd      elife
```

Pull out the data.frame's


```r
x %>%
  ft_collect() %>% 
  pub_chunks(c("doi", "publisher", "author")) %>%
  pub_tabularize() %>%
  .$elife
#> $`10.7554/eLife.03032`
#>                   doi                        publisher authors.given_names
#> 1 10.7554/eLife.03032 eLife Sciences Publications, Ltd                  Ya
#>   authors.surname authors.given_names.1 authors.surname.1
#> 1            Zhao                 Jimin               Lin
#>   authors.given_names.2 authors.surname.2 authors.given_names.3
#> 1               Beiying                Xu                  Sida
#>   authors.surname.3 authors.given_names.4 authors.surname.4
#> 1                Hu                   Xue             Zhang
#>   authors.given_names.5 authors.surname.5 .publisher
#> 1                Ligang                Wu      elife
#> 
#> $`10.7554/eLife.32763`
#>                   doi                        publisher authors.given_names
#> 1 10.7554/eLife.32763 eLife Sciences Publications, Ltd             Natasha
#>   authors.surname authors.given_names.1 authors.surname.1
#> 1          Mhatre                Robert            Malkin
#>   authors.given_names.2 authors.surname.2 authors.given_names.3
#> 1                Rittik               Deb                Rohini
#>   authors.surname.3 authors.given_names.4 authors.surname.4 .publisher
#> 1      Balakrishnan                Daniel            Robert      elife
```

## Extract text from PDFs

There are going to be cases in which some results you find in `ft_search()` have full text available in text, xml, or other machine readable formats, but some may be open access, but only in pdf format. We have a series of convenience functions in this package to help extract text from pdfs, both locally and remotely.

Locally, using code adapted from the package `tm`, and two pdf to text parsing backends


```r
pdf <- system.file("examples", "example2.pdf", package = "fulltext")
ft_extract(pdf)
#> <document>/Library/Frameworks/R.framework/Versions/3.5/Resources/library/fulltext/examples/example2.pdf
#>   Title: pone.0107412 1..10
#>   Producer: Acrobat Distiller 9.0.0 (Windows); modified using iText 5.0.3 (c) 1T3XT BVBA
#>   Creation date: 2014-09-18
```

### Interoperability with other packages downstream


```r
cache_options_set(path = (td <- 'foobar'))
#> $cache
#> [1] TRUE
#> 
#> $backend
#> [1] "ext"
#> 
#> $path
#> [1] "/Users/sckott/Library/Caches/R/foobar"
#> 
#> $overwrite
#> [1] FALSE
res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), type = "pdf")
library(readtext)
x <- readtext::readtext(file.path(cache_options_get()$path, "*.pdf"))
```


```r
library(quanteda)
quanteda::corpus(x)
#> Corpus consisting of 2 documents and 0 docvars.
```

## Contributors

* Scott Chamberlain <http://github.com/sckott>
* Will Pearse <https://github.com/willpearse>
* Katrin Leinweber <https://github.com/katrinleinweber>

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/fulltext/issues).
* License: MIT
* Get citation information for `fulltext`: `citation(package = 'fulltext')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

[suppdata]: https://github.com/ropensci/suppdata
[pubchunks]: https://github.com/ropensci/pubchunks
