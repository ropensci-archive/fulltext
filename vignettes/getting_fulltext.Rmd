<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Getting full text}
%\VignetteEncoding{UTF-8}
-->



The main interface to fetching full text is through `ft_get()`.

## Load fulltext


```r
library("fulltext")
```

## Structure of the returned object from ft_get()

Simple call, pass in a DOI and say where you want to get data from (by default, it's _plos_)


```r
res <- ft_get('10.1371/journal.pone.0086169', from = 'plos')
```

The article text and metadata is stored in the output object.

The `res` object is a list, with slots for each of the data sources, b/c you can request 
data from more than 1 data source.


```r
names(res)
#> [1] "plos"          "entrez"        "elife"         "pensoft"       "arxiv"         "biorxiv"       "elsevier"      "sciencedirect" "wiley"
```

Let's dig into the `plos` source object, which is another list, including metadata the 
text data itself (in the `data` slot).


```r
res$plos
#> $found
#> [1] 1
#> 
#> $dois
#> [1] "10.1371/journal.pone.0086169"
#> 
#> $data
#> $data$backend
#> [1] "ext"
#> 
#> $data$cache_path
#> [1] "/Users/sckott/Library/Caches/R/fulltext"
#> 
#> $data$path
#> $data$path$`10.1371/journal.pone.0086169`
#> $data$path$`10.1371/journal.pone.0086169`$path
#> [1] "/Users/sckott/Library/Caches/R/fulltext/10_1371_journal_pone_0086169.xml"
#> 
#> $data$path$`10.1371/journal.pone.0086169`$id
#> [1] "10.1371/journal.pone.0086169"
#> 
#> $data$path$`10.1371/journal.pone.0086169`$type
#> [1] "xml"
#> 
#> $data$path$`10.1371/journal.pone.0086169`$error
#> NULL
#> 
#> 
#> 
#> $data$data
#> NULL
#> 
#> 
#> $opts
#> $opts$doi
#> [1] "10.1371/journal.pone.0086169"
#> 
#> $opts$type
#> [1] "xml"
#> 
#> $opts$progress
#> [1] FALSE
#> 
#> 
#> $errors
#>                             id error
#> 1 10.1371/journal.pone.0086169  <NA>
```

Indexing to the `data` slot takes us to another list with metadata and the article


```r
res$plos$data
#> $backend
#> [1] "ext"
#> 
#> $cache_path
#> [1] "/Users/sckott/Library/Caches/R/fulltext"
#> 
#> $path
#> $path$`10.1371/journal.pone.0086169`
#> $path$`10.1371/journal.pone.0086169`$path
#> [1] "/Users/sckott/Library/Caches/R/fulltext/10_1371_journal_pone_0086169.xml"
#> 
#> $path$`10.1371/journal.pone.0086169`$id
#> [1] "10.1371/journal.pone.0086169"
#> 
#> $path$`10.1371/journal.pone.0086169`$type
#> [1] "xml"
#> 
#> $path$`10.1371/journal.pone.0086169`$error
#> NULL
#> 
#> 
#> 
#> $data
#> NULL
```

Going down one more index gets us the data object. There is no actual text as we have to 
collect it from the file on disk. See `ft_collect()` to get the text.


```r
res$plos$data
#> $backend
#> [1] "ext"
#> 
#> $cache_path
#> [1] "/Users/sckott/Library/Caches/R/fulltext"
#> 
#> $path
#> $path$`10.1371/journal.pone.0086169`
#> $path$`10.1371/journal.pone.0086169`$path
#> [1] "/Users/sckott/Library/Caches/R/fulltext/10_1371_journal_pone_0086169.xml"
#> 
#> $path$`10.1371/journal.pone.0086169`$id
#> [1] "10.1371/journal.pone.0086169"
#> 
#> $path$`10.1371/journal.pone.0086169`$type
#> [1] "xml"
#> 
#> $path$`10.1371/journal.pone.0086169`$error
#> NULL
#> 
#> 
#> 
#> $data
#> NULL
```

## Fetching many articles

You can get a bunch of DOIs first, e.g., from PLOS using the `rplos` package


```r
library("rplos")
(dois <- searchplos(q = "*:*", fl = 'id',
   fq = list('doc_type:full', "article_type:\"research article\""), limit = 5)$data$id)
#> [1] "10.1371/journal.pone.0020843" "10.1371/journal.pone.0022257" "10.1371/journal.pone.0023139" "10.1371/journal.pone.0023138" "10.1371/journal.pone.0023119"
ft_get(dois, from = 'plos')
#> <fulltext text>
#> [Docs] 5 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.1371/journal.pone.0020843 10.1371/journal.pone.0022257 10.1371/journal.pone.0023139 10.1371/journal.pone.0023138 10.1371/journal.pone.0023119 ...
```

## Different data sources

### Articles from eLife

One article


```r
ft_get('10.7554/eLife.04300', from = 'elife')
#> <fulltext text>
#> [Docs] 1 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.7554/eLife.04300 ...
```

Many articles


```r
ft_get(c('10.7554/eLife.04300','10.7554/eLife.03032'), from = 'elife')
#> <fulltext text>
#> [Docs] 2 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.7554/eLife.04300 10.7554/eLife.03032 ...
```

### Articles from Frontiers in Pharmacology (publisher: Frontiers)


```r
doi <- '10.3389/fphar.2014.00109'
ft_get(doi, from = "entrez")
#> <fulltext text>
#> [Docs] 1 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 4050532 ...
```

## Search using ft_search()

For example, search entrez, get some DOIs, then fetch some articles


```r
(res <- ft_search(query = 'ecology', from = 'entrez'))
#> Query:
#>   [ecology] 
#> Found:
#>   [PLoS: 0; BMC: 0; Crossref: 0; Entrez: 185437; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0] 
#> Returned:
#>   [PLoS: 0; BMC: 0; Crossref: 0; Entrez: 10; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0]
res$entrez$data$doi
#>  [1] "10.3748/wjg.v25.i45.6579"    "10.1038/s41467-019-13800-3"  "10.7554/eLife.49826"         "10.5365/wpsar.2018.9.5.005"  "10.1038/s41598-019-55147-1" 
#>  [6] "10.1038/s41598-019-54857-w"  "10.1038/s41598-019-55028-7"  "10.1038/s41598-019-55204-9"  "10.5423/PPJ.OA.05.2019.0129" "10.5423/PPJ.NT.08.2019.0216"
```

Get articles


```r
ft_get(res$entrez$data$doi[1:3], from = 'entrez')
#> <fulltext text>
#> [Docs] 3 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 6906207 6904573 6904214 ...
```

## Collect full text from file on disk

When using `ft_get()` you write the files to disk, and you have to pull text out of them as a 
separate step.


```r
(res <- ft_get('10.1371/journal.pone.0086169', from = 'plos'))
#> <fulltext text>
#> [Docs] 1 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.1371/journal.pone.0086169 ...
```

One way to do that is with `ft_collect()`. Before running `ft_collect()` the `data` slot is `NULL`.


```r
res$plos$data$data
#> NULL
```

Run `ft_collect()`


```r
res <- res %>% ft_collect
```

After running `ft_collect()` the `data` slot has the text. If there's more than one article they are named
by the identifier


```r
res$plos$data$data
#> $`10.1371/journal.pone.0086169`
#> {xml_document}
#> <article article-type="research-article" dtd-version="3.0" lang="en" xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:xlink="http://www.w3.org/1999/xlink">
#> [1] <front>\n  <journal-meta>\n    <journal-id journal-id-type="nlm-ta">PLoS ONE</journal-id>\n    <journal-id journal-id-type="publisher-id">plos</journal-id>\n    <journal-id j ...
#> [2] <body>\n  <sec id="s1">\n    <title>Introduction</title>\n    <p>Since the 1970s, biologists have directed substantial research effort to understanding adaptive sex-biased al ...
#> [3] <back>\n  <ack>\n    <p>We thank Joan Silk, Julienne Rutherford, and two anonymous reviewers for their comments on earlier drafts of the manuscript.</p>\n  </ack>\n  <ref-lis ...
```

