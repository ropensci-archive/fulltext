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
#> Query:
#>   [ecology] 
#> Found:
#>   [PLoS: 45067; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0] 
#> Returned:
#>   [PLoS: 10; BMC: 0; Crossref: 0; Entrez: 0; arxiv: 0; biorxiv: 0; Europe PMC: 0; Scopus: 0; Microsoft: 0]
```

Each publisher/search-engine has a slot with metadata and data


```r
res1$plos
#> Query: [ecology] 
#> Records found, returned: [45067, 10] 
#> License: [CC-BY] 
#>                              id
#> 1  10.1371/journal.pone.0001248
#> 2  10.1371/journal.pone.0059813
#> 3  10.1371/journal.pone.0155019
#> 4  10.1371/journal.pone.0080763
#> 5  10.1371/journal.pone.0150648
#> 6  10.1371/journal.pcbi.1003594
#> 7  10.1371/journal.pone.0102437
#> 8  10.1371/journal.pone.0175014
#> 9  10.1371/journal.pone.0166559
#> 10 10.1371/journal.pone.0054689
```

## Get full text

Using the results from `ft_search()` we can grab full text of some articles


```r
(out <- ft_get(res1))
#> <fulltext text>
#> [Docs] 10 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.1371/journal.pone.0001248 10.1371/journal.pone.0059813
#>      10.1371/journal.pone.0155019 10.1371/journal.pone.0080763
#>      10.1371/journal.pone.0150648 10.1371/journal.pcbi.1003594
#>      10.1371/journal.pone.0102437 10.1371/journal.pone.0175014
#>      10.1371/journal.pone.0166559 10.1371/journal.pone.0054689 ...
```

Dig in to the PLOS data


```r
out$plos$data$path[[1]]
#> $path
#> [1] "/Users/sckott/Library/Caches/R/fulltext/10_1371_journal_pone_0001248.xml"
#> 
#> $id
#> [1] "10.1371/journal.pone.0001248"
#> 
#> $type
#> [1] "xml"
#> 
#> $error
#> NULL
```

## Extract text from pdfs

Ideally for text mining you have access to XML or other text based formats. However, 
sometimes you only have access to PDFs. In this case you want to extract text 
from PDFs. `fulltext` can help with that. 

You can extract from any pdf from a file path, like:


```r
path <- system.file("examples", "example1.pdf", package = "fulltext")
ft_extract(path)
#> <document>/Library/Frameworks/R.framework/Versions/3.5/Resources/library/fulltext/examples/example1.pdf
#>   Title: Suffering and mental health among older people living in nursing homes---a mixed-methods study
#>   Producer: pdfTeX-1.40.10
#>   Creation date: 2015-07-17
```

Let's search for articles from arXiv, a preprint service. Here, get pdf from 
an article with ID `cond-mat/9309029`:


```r
res <- ft_get('cond-mat/9309029', from = "arxiv")
res2 <- ft_extract(res)
# the first page
res2$arxiv$data$data$`cond-mat/9309029`[1]
#> [1] "                                                                                                                          FERMILAB-PUB-93/15-T\n                                                                                                                                       March 1993\n                                                                                                                             Revised: January 1994\n                                           The Thermodynamics and Economics of Waste\n                                           Dallas C. Kennedy, Research Associate,\narXiv:cond-mat/9309029v8 26 Jan 1994\n                                           Fermi National Accelerator Laboratory,\n                                           P.O. Box 500 MS106, Batavia, Illinois 60510 USA∗\n                                                                                         Abstract\n                                       The increasingly relevant problem of natural resource use and waste production, disposal, and reuse is\n                                       examined from several viewpoints: economic, technical, and thermodynamic. Alternative economies are\n                                       studied, with emphasis on recycling of waste to close the natural resource cycle. The physical nature of\n                                       human economies and constraints on recycling and energy efficiency are stated in terms of entropy and\n                                       complexity.\n                                           What is a cynic? A man who knows the price of everything, and the value of nothing.\n                                                                                                                                      Oscar Wilde [1]\n                                            Our planet is finite in size and, except for a few energy and matter flows, its biosphere forms a closed\n                                       system. The envelope that makes life possible extends a small distance below the surface of the Earth and\n                                       less than a hundred miles into the atmosphere. While almost closed, the biosphere is not static, but is\n                                       constantly changing, moving flows of energy, air, water, soil, and life around in a shifting, never-repeating\n                                       pattern. The combination of general physical laws and the specific properties of the Earth places important\n                                       constraints on the activities of life, some embodied in the metabolism and forms of living creatures, others\n                                       imprinted into their genes by selective effects. All life needs sources of energy for sustenance and imposes\n                                       a burden of waste on its environment. Since this waste is usually harmful to the creatures emitting it, the\n                                       environment must, if these creatures are to continue living, break the waste down into less toxic forms and\n                                       possibly reuse it.\n                                            The growing dominance of humankind over the planet, both by technological power and by numbers,\n                                       imposes certain costs on the biosphere, sometimes a result of conscious attempts at controlling Nature, but\n                                       more frequently by unwitting influence. Moreover, the burden of carrying the activities of human sustenance,\n                                       unlike that of other animals, cannot be understood by considering the physical and biological activities of each\n                                       person in isolation. Because of their unique position at the top of the food chain, their tool-making abilities,\n                                       and the co-operative character of human activities (the division of labor or specialization), the physical and\n                                       biological aspects must be considered together in the context of the peculiarities of economic life [2]. The\n                                       economic aspects take on an independent importance because of the absence of any automatic, given means\n                                       of human subsistence and the extension of individual self-sufficiency by surplus production and trading [3].\n                                       This point of view is necessary for comprehending the ecological significance of all economies more elaborate\n                                       than the simplest subsistence or household economies, up to and including the most sophisticated systems\n                                       of technology and trade. On the other hand, the formulation of economic theory has generally taken place,\n                                         * Present address: Department of Physics, University of Florida, Gainesville, Florida 32611 USA\n                                                                                              1\n"
```

## Extract text chunks

We have a few functions to help you pull out certain parts of an article. 
For example, perhaps you want to get just the authors from your articles, 
or just the abstracts. 

Here, we'll search for some PLOS articles, then get their full text, then
extract various parts of each article with `ft_chunks()`.


```r
res <- ft_search(query = "ecology", from = "plos")
(x <- ft_get(res))
#> <fulltext text>
#> [Docs] 10 
#> [Source] ext - /Users/sckott/Library/Caches/R/fulltext 
#> [IDs] 10.1371/journal.pone.0001248 10.1371/journal.pone.0059813
#>      10.1371/journal.pone.0155019 10.1371/journal.pone.0080763
#>      10.1371/journal.pone.0150648 10.1371/journal.pcbi.1003594
#>      10.1371/journal.pone.0102437 10.1371/journal.pone.0175014
#>      10.1371/journal.pone.0166559 10.1371/journal.pone.0054689 ...
```

Extract DOIs


```r
x %>% ft_collect() %>% ft_chunks("doi")
#> $plos
#> $plos$`10.1371/journal.pone.0001248`
#> $plos$`10.1371/journal.pone.0001248`$doi
#> [1] "10.1371/journal.pone.0001248"
#> 
#> 
#> $plos$`10.1371/journal.pone.0059813`
#> $plos$`10.1371/journal.pone.0059813`$doi
#> [1] "10.1371/journal.pone.0059813"
#> 
#> 
#> $plos$`10.1371/journal.pone.0155019`
#> $plos$`10.1371/journal.pone.0155019`$doi
#> [1] "10.1371/journal.pone.0155019"
#> 
#> 
#> $plos$`10.1371/journal.pone.0080763`
#> $plos$`10.1371/journal.pone.0080763`$doi
#> [1] "10.1371/journal.pone.0080763"
#> 
#> 
#> $plos$`10.1371/journal.pone.0150648`
#> $plos$`10.1371/journal.pone.0150648`$doi
#> [1] "10.1371/journal.pone.0150648"
#> 
#> 
#> $plos$`10.1371/journal.pcbi.1003594`
#> $plos$`10.1371/journal.pcbi.1003594`$doi
#> [1] "10.1371/journal.pcbi.1003594"
#> 
#> 
#> $plos$`10.1371/journal.pone.0102437`
#> $plos$`10.1371/journal.pone.0102437`$doi
#> [1] "10.1371/journal.pone.0102437"
#> 
#> 
#> $plos$`10.1371/journal.pone.0175014`
#> $plos$`10.1371/journal.pone.0175014`$doi
#> [1] "10.1371/journal.pone.0175014"
#> 
#> 
#> $plos$`10.1371/journal.pone.0166559`
#> $plos$`10.1371/journal.pone.0166559`$doi
#> [1] "10.1371/journal.pone.0166559"
#> 
#> 
#> $plos$`10.1371/journal.pone.0054689`
#> $plos$`10.1371/journal.pone.0054689`$doi
#> [1] "10.1371/journal.pone.0054689"
```

Extract DOIs and categories


```r
x %>% ft_collect() %>% ft_chunks(c("doi", "title"))
#> $plos
#> $plos$`10.1371/journal.pone.0001248`
#> $plos$`10.1371/journal.pone.0001248`$doi
#> [1] "10.1371/journal.pone.0001248"
#> 
#> $plos$`10.1371/journal.pone.0001248`$title
#> [1] "A Tale of Four Stories: Soil Ecology, Theory, Evolution and the Publication System"
#> 
#> 
#> $plos$`10.1371/journal.pone.0059813`
#> $plos$`10.1371/journal.pone.0059813`$doi
#> [1] "10.1371/journal.pone.0059813"
#> 
#> $plos$`10.1371/journal.pone.0059813`$title
#> [1] "Trends in Ecological Research during the Last Three Decades – A Systematic Review"
#> 
#> 
#> $plos$`10.1371/journal.pone.0155019`
#> $plos$`10.1371/journal.pone.0155019`$doi
#> [1] "10.1371/journal.pone.0155019"
#> 
#> $plos$`10.1371/journal.pone.0155019`$title
#> [1] "Focusing Conservation Efforts on Ecosystem Service Supply May Increase Vulnerability of Socio-Ecological Systems"
#> 
#> 
#> $plos$`10.1371/journal.pone.0080763`
#> $plos$`10.1371/journal.pone.0080763`$doi
#> [1] "10.1371/journal.pone.0080763"
#> 
#> $plos$`10.1371/journal.pone.0080763`$title
#> [1] "Disproportional Representation of Primates in the Ecological Literature"
#> 
#> 
#> $plos$`10.1371/journal.pone.0150648`
#> $plos$`10.1371/journal.pone.0150648`$doi
#> [1] "10.1371/journal.pone.0150648"
#> 
#> $plos$`10.1371/journal.pone.0150648`$title
#> [1] "Quantifying Ecological Literacy in an Adult Western Community: The Development and Application of a New Assessment Tool and Community Standard"
#> 
#> 
#> $plos$`10.1371/journal.pcbi.1003594`
#> $plos$`10.1371/journal.pcbi.1003594`$doi
#> [1] "10.1371/journal.pcbi.1003594"
#> 
#> $plos$`10.1371/journal.pcbi.1003594`$title
#> [1] "Ecological Consistency of SSU rRNA-Based Operational Taxonomic Units at a Global Scale"
#> 
#> 
#> $plos$`10.1371/journal.pone.0102437`
#> $plos$`10.1371/journal.pone.0102437`$doi
#> [1] "10.1371/journal.pone.0102437"
#> 
#> $plos$`10.1371/journal.pone.0102437`$title
#> [1] "Combining Aesthetic with Ecological Values for Landscape Sustainability"
#> 
#> 
#> $plos$`10.1371/journal.pone.0175014`
#> $plos$`10.1371/journal.pone.0175014`$doi
#> [1] "10.1371/journal.pone.0175014"
#> 
#> $plos$`10.1371/journal.pone.0175014`$title
#> [1] "75 years of dryland science: Trends and gaps in arid ecology literature"
#> 
#> 
#> $plos$`10.1371/journal.pone.0166559`
#> $plos$`10.1371/journal.pone.0166559`$doi
#> [1] "10.1371/journal.pone.0166559"
#> 
#> $plos$`10.1371/journal.pone.0166559`$title
#> [1] "Eliciting and Representing High-Level Knowledge Requirements to Discover Ecological Knowledge in Flower-Visiting Data"
#> 
#> 
#> $plos$`10.1371/journal.pone.0054689`
#> $plos$`10.1371/journal.pone.0054689`$doi
#> [1] "10.1371/journal.pone.0054689"
#> 
#> $plos$`10.1371/journal.pone.0054689`$title
#> [1] "Representation of Ecological Systems within the Protected Areas Network of the Continental United States"
```

`ft_tabularize` attempts to help you put the data that comes out of 
`ft_chunks()` in to a `data.frame`, that we all know and love. 


```r
x %>% ft_collect() %>% ft_chunks(c("doi", "history")) %>% ft_tabularize()
#> $plos
#>                             doi history.received history.accepted
#> 1  10.1371/journal.pone.0001248       2007-07-02       2007-11-06
#> 2  10.1371/journal.pone.0059813       2012-09-16       2013-02-19
#> 3  10.1371/journal.pone.0155019       2015-09-22       2016-04-22
#> 4  10.1371/journal.pone.0080763       2013-08-15       2013-10-16
#> 5  10.1371/journal.pone.0150648       2015-09-19       2016-02-16
#> 6  10.1371/journal.pcbi.1003594       2014-01-09       2014-03-14
#> 7  10.1371/journal.pone.0102437       2013-11-27       2014-06-19
#> 8  10.1371/journal.pone.0175014       2016-09-23       2017-03-20
#> 9  10.1371/journal.pone.0166559       2016-06-05       2016-10-30
#> 10 10.1371/journal.pone.0054689       2012-03-22       2012-12-17
```

