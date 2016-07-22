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

The article text and metadata is stored in the output object (though soon they'll be the 
option to cache data on disk instead of in memory, but the interface will work the same 
for examining the text and metadata)

The `res` object is a list, with slots for each of the data sources, b/c you can request 
data from more than 1 data source.


```r
names(res)
#> [1] "plos"    "entrez"  "elife"   "pensoft" "arxiv"   "biorxiv"
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
#> NULL
#> 
#> $data$path
#> [1] "session"
#> 
#> $data$data
#> 1 full-text articles retrieved 
#> Min. Length: 110717 - Max. Length: 110717 
#> DOIs: 10.1371/journal.pone.0086169 ... 
#> 
#> NOTE: extract xml strings like output['<doi>']
#> 
#> $opts
#> $opts$doi
#> [1] "10.1371/journal.pone.0086169"
```

Indexing to the `data` slot takes us to another list with metadata and the article


```r
res$plos$data
#> $backend
#> NULL
#> 
#> $path
#> [1] "session"
#> 
#> $data
#> 1 full-text articles retrieved 
#> Min. Length: 110717 - Max. Length: 110717 
#> DOIs: 10.1371/journal.pone.0086169 ... 
#> 
#> NOTE: extract xml strings like output['<doi>']
```

Going down one more index gets us the data object, using the DOI searched to get the text. 
However, since we're dealing with full text, this is a bit messy (just a snippet here)


```r
res$plos$data$data$`10.1371/journal.pone.0086169`
#> [1] "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE article\n  
#> PUBLIC \"-//NLM//DTD Journal Publishing DTD v3.0 20080202//EN\"
#> \"http://dtd.nlm.nih.gov/publishing/3.0/journalpublishing3.dtd\">\n<article
#> xmlns:mml=\"http://www.w3.org/1998/Math/MathML\" xmlns:xlink=\"
...
```

## Fetching many articles

You can get a bunch of DOIs first, e.g., from PLOS using the `rplos` package


```r
library("rplos")
(dois <- searchplos(q = "*:*", fl = 'id',
   fq = list('doc_type:full', "article_type:\"research article\""), limit = 5)$data$id)
#> [1] "10.1371/journal.pone.0063114" "10.1371/journal.pone.0039479"
#> [3] "10.1371/journal.pone.0003940" "10.1371/journal.pcbi.0030082"
#> [5] "10.1371/journal.pone.0051856"
ft_get(dois, from = 'plos')
#> <fulltext text>
#> [Docs] 5 
#> [Source] R session  
#> [IDs] 10.1371/journal.pone.0063114 10.1371/journal.pone.0039479
#>      10.1371/journal.pone.0003940 10.1371/journal.pcbi.0030082
#>      10.1371/journal.pone.0051856 ...
```

## Different data sources

### Articles from eLife

One article


```r
ft_get('10.7554/eLife.04300', from = 'elife')
#> <fulltext text>
#> [Docs] 1 
#> [Source] R session  
#> [IDs] 10.7554/eLife.04300 ...
ft_get(c('10.7554/eLife.04300','10.7554/eLife.03032'), from = 'elife')
#> <fulltext text>
#> [Docs] 2 
#> [Source] R session  
#> [IDs] 10.7554/eLife.04300 10.7554/eLife.03032 ...
```

Many articles


```r
ft_get(c('10.7554/eLife.04300','10.7554/eLife.03032'), from = 'elife')
#> <fulltext text>
#> [Docs] 2 
#> [Source] R session  
#> [IDs] 10.7554/eLife.04300 10.7554/eLife.03032 ...
```

### Articles from Frontiers in Pharmacology (publisher: Frontiers)


```r
doi <- '10.3389/fphar.2014.00109'
ft_get(doi, from = "entrez")
#> <fulltext text>
#> [Docs] 1 
#> [Source] R session  
#> [IDs] 10.3389/fphar.2014.00109 ...
```

## Search using ft_search()

For example, search entrez, get some DOIs, then fetch some articles


```r
(res <- ft_search(query = 'ecology', from = 'entrez'))
#> Query:
#>   [ecology] 
#> Found:
#>   [PLoS: 0; BMC: 0; Crossref: 0; Entrez: 109655; arxiv: 0; biorxiv: 0; Europe PMC: 0] 
#> Returned:
#>   [PLoS: 0; BMC: 0; Crossref: 0; Entrez: 10; arxiv: 0; biorxiv: 0; Europe PMC: 0]
res$entrez$data$doi
#>  [1] "10.1109/ICASSP.2016.7472887"   NA                             
#>  [3] "10.1186/s12936-016-1386-3"     "10.1186/s13071-016-1688-x"    
#>  [5] "10.7554/eLife.16415"           "10.1155/2016/6018686"         
#>  [7] "10.1155/2016/3654093"          "10.1080/20477724.2016.1180775"
#>  [9] "10.3389/fimmu.2016.00253"      "10.3389/fmicb.2016.01026"
```

Get articles


```r
ft_get(res$entrez$data$doi[1:3], from = 'entrez')
#> Error: These are probably not DOIs:
#> 
#> NA
```

## Caching

To cache results or not. If `cache=TRUE`, raw XML, or other format that article is 
in is written to disk, then pulled from disk when further manipulations are done on 
the data.


```r
cache_options_set(cache = TRUE)
(res <- ft_get('10.1371/journal.pone.0086169', from = 'plos'))
#> <fulltext text>
#> [Docs] 1 
#> [Source] rds - /Users/sacmac/.fulltext 
#> [IDs] 10.1371/journal.pone.0086169 ...
```

> Note how the [source] field has "rds - <path to file>" - indicating that the text is 
cached on disk, not in R'm memory.

Nothing changes from the normal workflow now that data is cached on disk - simply behave 
normally as above. For example, `collect()` reads text from disk into memory (although 
the printed object doesn't indicate it)


```r
res %>% collect
#> <fulltext text>
#> [Docs] 1 
#> [Source] rds - /Users/sacmac/.fulltext 
#> [IDs] 10.1371/journal.pone.0086169 ...
```
