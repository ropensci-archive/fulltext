

fulltext
========

[![cran checks](https://cranchecks.info/badges/flavor/release/fulltext)](https://cranchecks.info/pkgs/fulltext)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-check](https://github.com/ropensci/fulltext/workflows/R-check/badge.svg)](https://github.com/ropensci/fulltext/actions/)
[![codecov](https://codecov.io/gh/ropensci/fulltext/branch/master/graph/badge.svg)](https://codecov.io/gh/ropensci/fulltext)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/fulltext)](https://github.com/r-hub/cranlogs.app)
[![cran version](https://www.r-pkg.org/badges/version/fulltext)](https://cran.r-project.org/package=fulltext)

__Get full text research articles__

Checkout the [package docs][docs] and the [fulltext manual][ftbook] to get started.

-----

rOpenSci has a number of R packages to get either full text, metadata, or both from various publishers. The goal of `fulltext` is to integrate these packages to create a single interface to many data sources.

`fulltext` makes it easy to do text-mining by supporting the following steps:

* Search for articles - `ft_search`
* Fetch articles - `ft_get`
* Get links for full text articles (xml, pdf) - `ft_links`
* Extract text from articles / convert formats - `ft_extract`
* Collect all texts into a data.frame - `ft_table`

Previously supported use cases, extracted out to other packages:

* Collect bits of articles that you actually need - moved to package `pubchunks`
* Supplementary data from papers has been moved to the `suppdata`


It's easy to go from the outputs of `ft_get` to text-mining packages such as 
`tm` and `quanteda`

Data sources in `fulltext` include:

* Crossref - via the `rcrossref` package
* Public Library of Science (PLOS) - via the `rplos` package
* Biomed Central
* arXiv - via the `aRxiv` package
* bioRxiv - via the `biorxivr` package
* PMC/Pubmed via Entrez - via the `rentrez` package
* Scopus - internal tooling
* Semantic Scholar - internal tooling
* Many more are supported via the above sources (e.g., _Royal Society Open Science_ is
available via Pubmed)
* We __will__ add more, as publishers open up, and as we have time...See the issues

Authentication: A number of publishers require authentication via API key, and some even more
draconian authentication processes involving checking IP addresses. We are working on supporting
all the various authentication things for different publishers, but of course all the OA content
is already easily available. See the **Authentication** section in `?fulltext-package` after 
loading the package.

We'd love your feedback. Let us know what you think in the issue tracker


## Installation

Stable version from CRAN


```r
install.packages("fulltext")
```

Development version from GitHub


```r
remotes::install_github("ropensci/fulltext")
```

Load library


```r
library('fulltext')
```

## Interoperability with other packages downstream

Note: this example not included in vignettes as that would require the two below packages in Suggests here. To see many examples and documentation see the [package docs][docs] and the [fulltext manual][ftbook].


```r
cache_options_set(path = (td <- 'foobar'))
res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), type = "pdf")
library(readtext)
x <- readtext::readtext(file.path(cache_options_get()$path, "*.pdf"))
```


```r
library(quanteda)
quanteda::corpus(x)
```

## Contributors

* Scott Chamberlain
* Will Pearse
* Katrin Leinweber

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/fulltext/issues).
* License: MIT
* Get citation information for `fulltext`: `citation(package = 'fulltext')`
* Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.


[docs]: https://docs.ropensci.org/fulltext/
[ftbook]: https://books.ropensci.org/fulltext/
