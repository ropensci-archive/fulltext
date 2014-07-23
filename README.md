## fulltext ~ Get full text across all da journals

rOpenSci has a number of R packages to get either full text, metadata, or both from various publishers. The goal of `fulltext` is to integrate these packages to create a single interface to many data sources.

Data sources currently available in `fulltext`:

* Public Library of Science (PLOS) - via `rplos`
* eLife - via `elife`
* Biomed Central - via `bmc`
* We hope to add more, as publishers open up, and as we have time...

## Installation

```r
install.packages("bmc")
devtools::install_github("ropensci/rplos")
devtools::install_github("ropensci/fulltext")
```

```r
library('fulltext')
```

## fulltext interface

Almost all functions are prefixed with `ft_`. Because functions in R are not forced to be namespaced, this makes conflict with other packages very unlikely.  

### Metadata

Sometimes you just want to find out how many things were found in your search. While you can set records to return to zero for database searches, this makes it dead simple. That's what `ft_meta()` does:

```r
ft_meta(query='reproducible science', from=c('plos','elife'))
```

### Search

While `ft_meta()` only returns metadata, `ft_search()` does the same search against the API service, but returns data as well.

```r
ft_search(query='reproducible science', from=c('plos','elife'))
```

### Cache

When dealing with full text data, you can get a lot quickly, and it can take a long time to get. That's where caching comes in. When you search you can optionally cache the raw JSON/XML/etc. of each request locally - when you do that exact search again we'll just give you the local data - unless of course you want new data, which you can do.

```r
ft_search(query='reproducible science', from=c('plos','elife'), cache=TRUE)
```

### Visualize

`ft_plot()` is a convenience function to plot the results of a call to `ft_search()`. There are a variety of options available, including common ways that one vizualizes full text data.

```r
res <- ft_search(query='reproducible science', from=c('plos','elife'))
ft_plot(res, method='somemethod')
```

(cool plot)

## The `ft` S3 object

The `ft` `S3` object is returned by a call to `ft_search()`. The object has a print method to give an informative summary of all data returned. Example:

```
Query ["reproducible science"]
No. records found [plos:4500, elife:150]
No. records returned [plos:20, elife:20]
First record [format: xml]:

<response>
<result name="response" numFound="22053" start="0" maxScore="0.9797668">
<doc>
<str name="id">10.1371/journal.pone.0059813</str>
<str name="journal">PLoS ONE</str>
<str name="eissn">1932-6203</str>
<date name="publication_date">2013-04-24T00:00:00Z</date>
<str name="article_type">Research Article</str>
...
```

Slots can be accessed for `meta`, `query`, and `results`.

## tm package integration

The `tm` package has a large suite of functions for working with text data. Instead of re-inventing the wheel, we integrate tm functions in this package.
