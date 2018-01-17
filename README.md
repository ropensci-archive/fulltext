

```
  _____     .__  .__   __                   __
_/ ____\_ __|  | |  |_/  |_  ____ ___  ____/  |_
\   __\  |  \  | |  |\   __\/ __ \\  \/  /\   __\
 |  | |  |  /  |_|  |_|  | \  ___/ >    <  |  |
 |__| |____/|____/____/__|  \___  >__/\_ \ |__|
                                \/      \/
```

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://api.travis-ci.org/ropensci/fulltext.svg)](https://travis-ci.org/ropensci/fulltext)
[![Build status](https://ci.appveyor.com/api/projects/status/y487h3ec5wc2s20m/branch/master?svg=true)](https://ci.appveyor.com/project/sckott/fulltext/branch/master)
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
* Collect bits of articles that you actually need - `ft_chunks`/`ft_tabularize`
* Collect all texts into a data.frame - `ft_table`
* Download supplementary materials from papers - `ft_get_si`

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

Authorization: A number of publishers require authorization via API key, and some even more
draconian authorization processes involving checking IP addresses. We are working on supporting
all the various authorization things for different publishers, but of course all the OA content
is already easily available.

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
#>   [PLoS: 0; BMC: 0; Crossref: 144283; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0] 
#> Returned:
#>   [PLoS: 0; BMC: 0; Crossref: 10; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0]
```

## Get full text links

`ft_links()` - get links for articles (xml and pdf).


```r
res1 <- ft_search(query = 'ecology', from = 'entrez', limit = 5)
ft_links(res1)
#> <fulltext links>
#> [Found] 5 
#> [IDs] ID_29321529 ID_29321528 ID_29321519 ID_29321473 ID_29319501 ...
```

Or pass in DOIs directly


```r
ft_links(res1$entrez$data$doi, from = "entrez")
#> <fulltext links>
#> [Found] 5 
#> [IDs] ID_29321529 ID_29321528 ID_29321519 ID_29321473 ID_29319501 ...
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
x <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), from = "elife")
x %>% ft_collect() %>% ft_chunks("publisher") %>% ft_tabularize()
#> $elife
#>                          publisher
#> 1 eLife Sciences Publications, Ltd
#> 2 eLife Sciences Publications, Ltd
```

Get multiple fields at once


```r
x %>% ft_collect() %>% ft_chunks(c("doi","publisher")) %>% ft_tabularize()
#> $elife
#>                   doi                        publisher
#> 1 10.7554/eLife.03032 eLife Sciences Publications, Ltd
#> 2 10.7554/eLife.32763 eLife Sciences Publications, Ltd
```

Use `dplyr` to data munge


```r
library("dplyr")
x %>%
  ft_collect() %>% 
  ft_chunks(c("doi", "publisher", "permissions")) %>%
  ft_tabularize() %>%
  .$elife %>%
  select(-permissions.license, -permissions.license_url)
#>                   doi                        publisher
#> 1 10.7554/eLife.03032 eLife Sciences Publications, Ltd
#> 2 10.7554/eLife.32763 eLife Sciences Publications, Ltd
#>   permissions.copyright.statement permissions.copyright.year
#> 1              © 2014, Zhao et al                       2014
#> 2            © 2017, Mhatre et al                       2017
#>   permissions.copyright.holder permissions.free_to_read
#> 1                   Zhao et al                     <NA>
#> 2                 Mhatre et al
```

## Supplementary materials

Grab supplementary materials for (re-)analysis of data

`ft_get_si()` accepts article identifiers, and output from `ft_search()`, `ft_get()`


```r
catching.crabs <- read.csv(ft_get_si("10.6084/m9.figshare.979288", 2))
head(catching.crabs)
#>   trap.no. length.deployed no..crabs
#> 1        1          10 sec         0
#> 2        2          10 sec         0
#> 3        3          10 sec         0
#> 4        4          10 sec         0
#> 5        5          10 sec         0
#> 6        1           1 min         0
```

## Extract text from PDFs

There are going to be cases in which some results you find in `ft_search()` have full text available in text, xml, or other machine readable formats, but some may be open access, but only in pdf format. We have a series of convenience functions in this package to help extract text from pdfs, both locally and remotely.

Locally, using code adapted from the package `tm`, and two pdf to text parsing backends


```r
pdf <- system.file("examples", "example2.pdf", package = "fulltext")
```


```r
ft_extract(pdf)
#> <document>/Library/Frameworks/R.framework/Versions/3.4/Resources/library/fulltext/examples/example2.pdf
#>   Title: pone.0107412 1..10
#>   Producer: Acrobat Distiller 9.0.0 (Windows); modified using iText 5.0.3 (c) 1T3XT BVBA
#>   Creation date: 2014-09-18
```

### Interoperability with other packages downstream


```r
cache_options_set(path = (td <- 'foobar'))
res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), type = "pdf")
library(readtext)
x <- readtext::readtext(file.path(cache_options_get()$path, "*.pdf"))
```


```r
library(quanteda)
quanteda::corpus(x)
#> Corpus consisting of 2 documents and 1 docvar.
```

## Contributors

* Scott Chamberlain <http://github.com/sckott>
* Will Pearse <https://github.com/willpearse>

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/fulltext/issues).
* License: MIT
* Get citation information for `fulltext`: `citation(package = 'fulltext')`
* Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
