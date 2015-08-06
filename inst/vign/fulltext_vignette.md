<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{fulltext introduction}
%\VignetteEncoding{UTF-8}
-->

fulltext introduction
======

`fulltext` is a package to facilitate text mining. It focuses on open access journals. This package makes it easier to search for articles, download those articles in full text if available, convert pdf format to plain text, and extract text chunks for vizualization/analysis. We are planning to add bits for analysis in future versions. The steps in bullet form:

* Search
* Retrieve
* Convert
* Text
* Extract

## Load fulltext


```r
library("fulltext")
```

## Search for articles

Search for the term _ecology_ in PLOS journals.


```r
(res1 <- ft_search(query = 'ecology', from = 'plos'))
```

```
## Query:
##   [ecology] 
## Found:
##   [PLoS: 28561; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0] 
## Returned:
##   [PLoS: 10; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0]
```

Each publisher/search-engine has a slot with metadata and data


```r
res1$plos
```

```
## Query: [ecology] 
## Records found, returned: [28561, 10] 
## License: [CC-BY] 
##                                                         id
## 1                             10.1371/journal.pone.0059813
## 2                             10.1371/journal.pone.0001248
## 3  10.1371/annotation/69333ae7-757a-4651-831c-f28c5eb02120
## 4                             10.1371/journal.pone.0080763
## 5                             10.1371/journal.pone.0102437
## 6                             10.1371/journal.pone.0017342
## 7                             10.1371/journal.pone.0091497
## 8                             10.1371/journal.pone.0092931
## 9  10.1371/annotation/28ac6052-4f87-4b88-a817-0cd5743e83d6
## 10                            10.1371/journal.pcbi.1003594
```

## Get full text

Using the results from `ft_search()` we can grab full text of some articles


```r
(out <- ft_get(res1))
```

```
## [Docs] 8 
## [Source] R session  
## [IDs] 10.1371/journal.pone.0059813 10.1371/journal.pone.0001248
##      10.1371/journal.pone.0080763 10.1371/journal.pone.0102437
##      10.1371/journal.pone.0017342 10.1371/journal.pone.0091497
##      10.1371/journal.pone.0092931 10.1371/journal.pcbi.1003594 ...
```

Dig in to the PLOS data


```r
out$plos
```

```
## $found
## [1] 8
## 
## $dois
## [1] "10.1371/journal.pone.0059813" "10.1371/journal.pone.0001248"
## [3] "10.1371/journal.pone.0080763" "10.1371/journal.pone.0102437"
## [5] "10.1371/journal.pone.0017342" "10.1371/journal.pone.0091497"
## [7] "10.1371/journal.pone.0092931" "10.1371/journal.pcbi.1003594"
## 
## $data
## $data$backend
## NULL
## 
## $data$path
## [1] "session"
## 
## $data$data
## 8 full-text articles retrieved 
## Min. Length: 3828 - Max. Length: 104702 
## DOIs: 10.1371/journal.pone.0059813 10.1371/journal.pone.0001248
##   10.1371/journal.pone.0080763 10.1371/journal.pone.0102437
##   10.1371/journal.pone.0017342 10.1371/journal.pone.0091497
##   10.1371/journal.pone.0092931 10.1371/journal.pcbi.1003594 ... 
## 
## NOTE: extract xml strings like output['<doi>']
## 
## $opts
## $opts$doi
## [1] "10.1371/journal.pone.0059813" "10.1371/journal.pone.0001248"
## [3] "10.1371/journal.pone.0080763" "10.1371/journal.pone.0102437"
## [5] "10.1371/journal.pone.0017342" "10.1371/journal.pone.0091497"
## [7] "10.1371/journal.pone.0092931" "10.1371/journal.pcbi.1003594"
## 
## $opts$callopts
## list()
```

Dig in further to get to one of the articles in XML format


```r
library("xml2")
xml2::read_xml(out$plos$data$data$`10.1371/journal.pone.0059813`)
```

```
## {xml_document}
## <article>
## [1] <front>\n<journal-meta>\n<journal-id journal-id-type="nlm-ta">PLoS O ...
## [2] <body>\n  <sec id="s1">\n<title>Introduction</title>\n<p>Ecologists  ...
## [3] <back>\n<ack>\n<p>Curtis Flather, Mark Burgman, Leon Blaustein, Yaac ...
```

## Extract text from pdfs

Ideally for text mining you have access to XML or other text based formats. However, sometimes you only have access to PDFs. In this case you want to extract text from PDFs. `fulltext` can help with that. 

You can extract from any pdf from a file path, like:


```r
path <- system.file("examples", "example1.pdf", package = "fulltext")
ft_extract(path)
```

```
## <document>/Library/Frameworks/R.framework/Versions/3.2/Resources/library/fulltext/examples/example1.pdf
##   Pages: 18
##   Title: Suffering and mental health among older people living in nursing homes---a mixed-methods study
##   Producer: pdfTeX-1.40.10
##   Creation date: 2015-07-17
```

Let's search for articles from arXiv, a preprint service. Here, get pdf from an article with ID `cond-mat/9309029`:


```r
res <- ft_get('cond-mat/9309029', from = "arxiv")
res2 <- ft_extract(res)
res2$arxiv$data
```

```
## $backend
## NULL
## 
## $path
## $path$`cond-mat/9309029`
## [1] "~/.fulltext/cond-mat_9309029.pdf"
## 
## 
## $data
## $data[[1]]
## <document>/Users/sacmac/.fulltext/cond-mat_9309029.pdf
##   Pages: 14
##   Title: arXiv:cond-mat/9309029v8  26 Jan 1994
##   Producer: GPL Ghostscript SVN PRE-RELEASE 8.62
##   Creation date: 2008-02-06
```

And a short snippet of the full text


```r
res2$arxiv$data$data[[1]]$data
#> "arXiv:cond-mat/9309029v8 26 Jan 1994, , FERMILAB-PUB-93/15-T March 1993, Revised:
#> January 1994, The Thermodynamics and Economics of Waste, Dallas C. Kennedy, Research
#> Associate, Fermi National Accelerator Laboratory, P.O. Box 500 MS106, Batavia, Illinois
#> 60510 USA, Abstract, The increasingly relevant problem of natural resource use and
#> waste production, disposal, and reuse is examined from several viewpoints: economic,
#> technical, and thermodynamic. Alternative economies are studied, with emphasis on
#> recycling of waste to close the natural resource cycle. The physical nature of human
#> economies and constraints on recycling and energy efficiency are stated in terms 
#> ..."
```

## Extract text chunks

We have a few functions to help you pull out certain parts of an article. For example, perhaps you want to get just the authors from your articles, or just the abstracts. 

Here, we'll search for some PLOS articles, then get their full text, then extract various parts of each article with `chunks()`.


```r
res <- ft_search(query = "ecology", from = "plos")
(x <- ft_get(res))
```

```
## [Docs] 8 
## [Source] R session  
## [IDs] 10.1371/journal.pone.0059813 10.1371/journal.pone.0001248 10.1371/journal.pone.0080763
##      10.1371/journal.pone.0102437 10.1371/journal.pone.0017342 10.1371/journal.pone.0091497
##      10.1371/journal.pone.0092931 10.1371/journal.pcbi.1003594 ...
```

Extract DOIs


```r
x %>% chunks("doi")
```

```
## $plos
## $plos$`10.1371/journal.pone.0059813`
## $plos$`10.1371/journal.pone.0059813`$doi
## [1] "10.1371/journal.pone.0059813"
## 
## 
## $plos$`10.1371/journal.pone.0001248`
## $plos$`10.1371/journal.pone.0001248`$doi
## [1] "10.1371/journal.pone.0001248"
## 
## 
## $plos$`10.1371/journal.pone.0080763`
## $plos$`10.1371/journal.pone.0080763`$doi
## [1] "10.1371/journal.pone.0080763"
## 
## 
## $plos$`10.1371/journal.pone.0102437`
## $plos$`10.1371/journal.pone.0102437`$doi
## [1] "10.1371/journal.pone.0102437"
## 
## 
## $plos$`10.1371/journal.pone.0017342`
## $plos$`10.1371/journal.pone.0017342`$doi
## [1] "10.1371/journal.pone.0017342"
## 
## 
## $plos$`10.1371/journal.pone.0091497`
## $plos$`10.1371/journal.pone.0091497`$doi
## [1] "10.1371/journal.pone.0091497"
## 
## 
## $plos$`10.1371/journal.pone.0092931`
## $plos$`10.1371/journal.pone.0092931`$doi
## [1] "10.1371/journal.pone.0092931"
## 
## 
## $plos$`10.1371/journal.pcbi.1003594`
## $plos$`10.1371/journal.pcbi.1003594`$doi
## [1] "10.1371/journal.pcbi.1003594"
```

Extract DOIs and categories


```r
x %>% chunks(c("doi","categories"))
```

```
## $plos
## $plos$`10.1371/journal.pone.0059813`
## $plos$`10.1371/journal.pone.0059813`$doi
## [1] "10.1371/journal.pone.0059813"
## 
## $plos$`10.1371/journal.pone.0059813`$categories
##  [1] "Research Article"                 "Biology"                          "Ecology"                         
##  [4] "Community ecology"                "Species interactions"             "Science policy"                  
##  [7] "Research assessment"              "Research monitoring"              "Research funding"                
## [10] "Government funding of science"    "Research laboratories"            "Science policy and economics"    
## [13] "Science and technology workforce" "Careers in research"              "Social and behavioral sciences"  
## [16] "Sociology"                        "Sociology of knowledge"          
## 
## 
## $plos$`10.1371/journal.pone.0001248`
## $plos$`10.1371/journal.pone.0001248`$doi
## [1] "10.1371/journal.pone.0001248"
## 
## $plos$`10.1371/journal.pone.0001248`$categories
## [1] "Research Article"             "Ecology"                      "Ecology/Ecosystem Ecology"   
## [4] "Ecology/Evolutionary Ecology" "Ecology/Theoretical Ecology" 
## 
## 
## $plos$`10.1371/journal.pone.0080763`
## $plos$`10.1371/journal.pone.0080763`$doi
## [1] "10.1371/journal.pone.0080763"
## 
## $plos$`10.1371/journal.pone.0080763`$categories
##  [1] "Research Article"     "Biology"              "Ecology"              "Autecology"          
##  [5] "Behavioral ecology"   "Community ecology"    "Evolutionary ecology" "Population ecology"  
##  [9] "Evolutionary biology" "Behavioral ecology"   "Evolutionary ecology" "Population biology"  
## [13] "Population ecology"  
## 
## 
## $plos$`10.1371/journal.pone.0102437`
## $plos$`10.1371/journal.pone.0102437`$doi
## [1] "10.1371/journal.pone.0102437"
## 
## $plos$`10.1371/journal.pone.0102437`$categories
##  [1] "Research Article"                   "Biology and life sciences"          "Biogeography"                      
##  [4] "Ecology"                            "Ecosystems"                         "Ecosystem engineering"             
##  [7] "Ecosystem functioning"              "Industrial ecology"                 "Spatial and landscape ecology"     
## [10] "Urban ecology"                      "Computer and information sciences"  "Geoinformatics"                    
## [13] "Spatial analysis"                   "Earth sciences"                     "Geography"                         
## [16] "Human geography"                    "Cultural geography"                 "Social geography"                  
## [19] "Ecology and environmental sciences" "Conservation science"               "Environmental protection"          
## [22] "Nature-society interactions"       
## 
## 
## $plos$`10.1371/journal.pone.0017342`
## $plos$`10.1371/journal.pone.0017342`$doi
## [1] "10.1371/journal.pone.0017342"
## 
## $plos$`10.1371/journal.pone.0017342`$categories
##  [1] "Research Article"     "Biology"              "Ecology"              "Community ecology"   
##  [5] "Community assembly"   "Community structure"  "Niche construction"   "Ecological metrics"  
##  [9] "Species diversity"    "Species richness"     "Biodiversity"         "Biogeography"        
## [13] "Population ecology"   "Mathematics"          "Statistics"           "Biostatistics"       
## [17] "Statistical theories" "Ecology"              "Mathematics"         
## 
## 
## $plos$`10.1371/journal.pone.0091497`
## $plos$`10.1371/journal.pone.0091497`$doi
## [1] "10.1371/journal.pone.0091497"
## 
## $plos$`10.1371/journal.pone.0091497`$categories
## [1] "Correction"
## 
## 
## $plos$`10.1371/journal.pone.0092931`
## $plos$`10.1371/journal.pone.0092931`$doi
## [1] "10.1371/journal.pone.0092931"
## 
## $plos$`10.1371/journal.pone.0092931`$categories
## [1] "Correction"
## 
## 
## $plos$`10.1371/journal.pcbi.1003594`
## $plos$`10.1371/journal.pcbi.1003594`$doi
## [1] "10.1371/journal.pcbi.1003594"
## 
## $plos$`10.1371/journal.pcbi.1003594`$categories
## [1] "Research Article"          "Biology and life sciences" "Computational biology"     "Microbiology"             
## [5] "Theoretical biology"
```

`tabularize` attempts to help you put the data that comes out of `chunks()` in to a `data.frame`, that we all know and love. 


```r
x %>% chunks(c("doi", "history")) %>% tabularize()
```

```
## $plos
##                            doi history.received history.accepted
## 1 10.1371/journal.pone.0059813       2012-09-16       2013-02-19
## 2 10.1371/journal.pone.0001248       2007-07-02       2007-11-06
## 3 10.1371/journal.pone.0080763       2013-08-15       2013-10-16
## 4 10.1371/journal.pone.0102437       2013-11-27       2014-06-19
## 5 10.1371/journal.pone.0017342       2010-08-24       2011-01-31
## 6 10.1371/journal.pone.0091497             <NA>             <NA>
## 7 10.1371/journal.pone.0092931             <NA>             <NA>
## 8 10.1371/journal.pcbi.1003594       2014-01-09       2014-03-14
```

