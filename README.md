

```
  _____     .__  .__   __                   __
_/ ____\_ __|  | |  |_/  |_  ____ ___  ____/  |_
\   __\  |  \  | |  |\   __\/ __ \\  \/  /\   __\
 |  | |  |  /  |_|  |_|  | \  ___/ >    <  |  |
 |__| |____/|____/____/__|  \___  >__/\_ \ |__|
                                \/      \/
```

__Get full text across all da (open access) journals__

[![Build Status](https://api.travis-ci.org/ropensci/fulltext.png)](https://travis-ci.org/ropensci/fulltext)
[![Build status](https://ci.appveyor.com/api/projects/status/y487h3ec5wc2s20m/branch/master?svg=true)](https://ci.appveyor.com/project/sckott/fulltext/branch/master)
[![codecov.io](https://codecov.io/github/ropensci/fulltext/coverage.svg?branch=master)](https://codecov.io/github/ropensci/fulltext?branch=master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/fulltext)](https://github.com/metacran/cranlogs.app)

rOpenSci has a number of R packages to get either full text, metadata, or both from various publishers. The goal of `fulltext` is to integrate these packages to create a single interface to many data sources.

`fulltext` attempts to make it easy to do text-mining by supporting the following steps:

* Search for articles
* Fetch articles
* Extract text from articles / convert formats
* Collect bits of articles that you actually need

Additional steps we hope to include in future versions:

* Analysis enable via the [tm](https://cran.rstudio.com/web/packages/tm/) package and friends, or via [Spark-R](https://amplab-extras.github.io/SparkR-pkg/)
* Visualization

Data sources in `fulltext` include:

* [Crossref](http://www.crossref.org/) - via the `rcrossref` package
* [Public Library of Science (PLOS)](https://www.plos.org/) - via the `rplos` package
* [Biomed Central](http://www.biomedcentral.com/)
* [arXiv](https://arxiv.org) - via the `aRxiv` package
* [bioRxiv](http://biorxiv.org/) - via the `biorxivr` package
* [PMC/Pubmed via Entrez](http://www.ncbi.nlm.nih.gov/) - via the `rentrez` package
* We __will__ add more, as publishers open up, and as we have time...See the [master list here](https://github.com/ropensci/fulltext/issues/4#issuecomment-52376743)

We'd love your feedback. Let us know what you think in [the issue tracker](https://github.com/ropensci/fulltext/issues).

Article full text formats by publisher:

* [https://github.com/ropensci/fulltext/blob/master/vignettes/formats.Rmd](https://github.com/ropensci/fulltext/blob/master/vignettes/formats.Rmd)

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
ft_search(query = 'ecology', from = 'plos')
#> Query:
#>   [ecology] 
#> Found:
#>   [PLoS: 28561; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0] 
#> Returned:
#>   [PLoS: 10; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0]
```

## Get full text

`ft_get()` - get full or partial text of articles.


```r
ft_get('10.1371/journal.pone.0086169', from = 'plos')
#> [Docs] 1 
#> [Source] R session  
#> [IDs] 10.1371/journal.pone.0086169 ...
```

## Extract chunks


```r
library("rplos")
(dois <- searchplos(q = "*:*", fl = 'id',
   fq = list('doc_type:full',"article_type:\"research article\""), limit = 5)$data$id)
#> [1] "10.1371/journal.pone.0031384" "10.1371/journal.pone.0031385"
#> [3] "10.1371/journal.pone.0107441" "10.1371/journal.pone.0000339"
#> [5] "10.1371/journal.pone.0046739"
x <- ft_get(dois, from = "plos")
x %>% chunks("publisher") %>% tabularize()
#> $plos
#>                                               publisher
#> 1 Public Library of Science\n        San Francisco, USA
#> 2 Public Library of Science\n        San Francisco, USA
#> 3         Public Library of Science\nSan Francisco, USA
#> 4           Public Library of ScienceSan Francisco, USA
#> 5 Public Library of Science\n        San Francisco, USA
```


```r
x %>% chunks(c("doi","publisher")) %>% tabularize()
#> $plos
#>                            doi
#> 1 10.1371/journal.pone.0031384
#> 2 10.1371/journal.pone.0031385
#> 3 10.1371/journal.pone.0107441
#> 4 10.1371/journal.pone.0000339
#> 5 10.1371/journal.pone.0046739
#>                                               publisher
#> 1 Public Library of Science\n        San Francisco, USA
#> 2 Public Library of Science\n        San Francisco, USA
#> 3         Public Library of Science\nSan Francisco, USA
#> 4           Public Library of ScienceSan Francisco, USA
#> 5 Public Library of Science\n        San Francisco, USA
```

Use `dplyr` to data munge


```r
library("dplyr")
x %>%
 chunks(c("doi", "publisher", "permissions")) %>%
 tabularize() %>%
 .$plos %>%
 select(-permissions.license)
#>                            doi
#> 1 10.1371/journal.pone.0031384
#> 2 10.1371/journal.pone.0031385
#> 3 10.1371/journal.pone.0107441
#> 4 10.1371/journal.pone.0000339
#> 5 10.1371/journal.pone.0046739
#>                                               publisher
#> 1 Public Library of Science\n        San Francisco, USA
#> 2 Public Library of Science\n        San Francisco, USA
#> 3         Public Library of Science\nSan Francisco, USA
#> 4           Public Library of ScienceSan Francisco, USA
#> 5 Public Library of Science\n        San Francisco, USA
#>   permissions.copyright.year permissions.copyright.holder
#> 1                       2012                 Arnold et al
#> 2                       2012                     Hu, Kuhn
#> 3                       2014           Peterson, McKenzie
#> 4                       2007                Shrager et al
#> 5                       2012                Kapoula et al
#>                       permissions.license_url
#> 1                                        <NA>
#> 2                                        <NA>
#> 3 http://creativecommons.org/licenses/by/4.0/
#> 4                                        <NA>
#> 5                                        <NA>
```

## Cache

When dealing with full text data, you can get a lot quickly, and it can take a long time to get. That's where caching comes in. And after you pull down a bunch of data, if you do so within the R session, you don't want to lose that data if the session crashes, etc. When you search you _will be able to_ (i.e., not ready yet) optionally cache the raw JSON/XML/etc. of each request locally - when you do that exact search again we'll just give you the local data - unless of course you want new data, which you can do.


```r
ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE)
```

## Extract text from PDFs

There are going to be cases in which some results you find in `ft_search()` have full text available in text, xml, or other machine readable formats, but some may be open access, but only in pdf format. We have a series of convenience functions in this package to help extract text from pdfs, both locally and remotely.

Locally, using code adapted from the package `tm`, and various pdf to text parsing backends


```r
pdf <- system.file("examples", "example2.pdf", package = "fulltext")
```

Using `ghostscript`


```r
(res_gs <- ft_extract(pdf, "gs"))
#> <document>/Library/Frameworks/R.framework/Versions/3.2/Resources/library/fulltext/examples/example2.pdf
#>   Title: pone.0107412 1..10
#>   Producer: Acrobat Distiller 9.0.0 (Windows); modified using iText 5.0.3 (c) 1T3XT BVBA
#>   Creation date: 2014-09-18
```

Using `xpdf`


```r
(res_xpdf <- ft_extract(pdf, "xpdf"))
#> <document>/Library/Frameworks/R.framework/Versions/3.2/Resources/library/fulltext/examples/example2.pdf
#>   Pages: 10
#>   Title: pone.0107412 1..10
#>   Producer: Acrobat Distiller 9.0.0 (Windows); modified using iText 5.0.3 (c) 1T3XT BVBA
#>   Creation date: 2014-09-18
```

Or extract directly into a `tm` Corpus


```r
paths <- sapply(paste0("example", 2:5, ".pdf"), function(x) system.file("examples", x, package = "fulltext"))
(corpus_xpdf <- ft_extract_corpus(paths, "xpdf"))
#> $meta
#>           names                           class
#> 1 content, meta PlainTextDocument, TextDocument
#> 2 content, meta PlainTextDocument, TextDocument
#> 3 content, meta PlainTextDocument, TextDocument
#> 4 content, meta PlainTextDocument, TextDocument
#> 
#> $data
#> <<VCorpus>>
#> Metadata:  corpus specific: 0, document level (indexed): 0
#> Content:  documents: 4
#> 
#> attr(,"class")
#> [1] "xpdf"
```

Extract pdf remotely on the web, using a service called `PDFX`


```r
pdf5 <- system.file("examples", "example5.pdf", package = "fulltext")
pdfx(file = pdf5)
```


```r
#> $meta
#> $meta$job
#> [1] "34b281c10730b9e777de8a29b2dbdcc19f7d025c71afe9d674f3c5311a1f2044"
#>
#> $meta$base_name
#> [1] "5kpp"
#>
#> $meta$doi
#> [1] "10.7554/eLife.03640"
#>
#>
#> $data
#> <?xml version="1.0" encoding="UTF-8"?>
#> <pdfx xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://pdfx.cs.man.ac.uk/static/article-schema.xsd">
#>   <meta>
#>     <job>34b281c10730b9e777de8a29b2dbdcc19f7d025c71afe9d674f3c5311a1f2044</job>
#>     <base_name>5kpp</base_name>
#>     <doi>10.7554/eLife.03640</doi>
#>   </meta>
#>    <article>
#>  .....
```

## TODO

* `ft_plot()` - vizualize metadata or full text data

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/fulltext/issues).
* License: MIT
* Get citation information for `fulltext`: `citation(package = 'fulltext')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

[![rofooter](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
