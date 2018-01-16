#' @title Fulltext search and retrieval of scholarly texts.
#'
#' @description fulltext is a single interface to many sources of scholarly
#' texts. In practice, this means only ones that are legally useable. We will
#' support sources that require authentication on a case by case basis - that
#' is, if more than just a few people will use it, and it's not too
#' burdensome to include, then we can include that source.
#' 
#' @section Manual:
#' See <https://ropensci.github.io/fulltext-book/> for a longer form manual for 
#' using \pkg{fulltext}.
#'
#' @section What's included:
#' We currently include support for search and full text retrieval for a variety
#' of publishers. See [ft_search()] for what we include for search,
#' and  [ft_get()] for what we include for full text retrieval.
#'
#' @section Use cases:
#' The following are tasks/use cases supported:
#' 
#' - search - [ft_search()]
#' - get texts - [ft_get()]
#' - get full text links - [ft_links()]
#' - get abstracts - [ft_abstract()]
#' - extract text from pdfs - [ft_extract()]
#' - serialize to different data formats - [ft_serialize()]
#' - extract certain article sections (e.g., authors) - [ft_chunks()]
#' - grab supplementary materials for (re-)analysis of data -
#'  [ft_get_si()] accepts article identifiers, and output from
#'  [ft_search()] and  [ft_get()]
#'
#' @section DOI delays:
#' Beware that DOIs are not searchable via Crossref/Entrez immediately. The
#' delay may be as much as a few days, though should be less than a day. This
#' delay should become shorter as services improve. The point of this is that
#' you man not find a match for a relatively new DOI (e.g., for an article
#' published the same day). We've tried to account for this for some publishers.
#' For example, for Crossref we search Crossref for a match for a DOI, and if
#' none is found we attempt to retrieve the full text from the publisher
#' directly.
#' 
#' @section Rate limits:
#' **Scopus**: 20,000 per 7 days. See 
#' <https://dev.elsevier.com/api_key_settings.html> for rate 
#' limit information. To see what your personal rate limit details are, 
#' request verbose HTTP request output - this will vary on the function
#' you are using - see the docs for the function. See the response 
#' headers `X-RateLimit-Limit`, `X-RateLimit-Remaining`, and 
#' `X-RateLimit-Reset` (your limit, those requests remaining, and UTC 
#' date/time it will reset)
#' 
#' **Microsoft**: 10,000 per month, and 1 per second. There are no rate 
#' limit headers, sorry :(
#' 
#' **PLOS**: There are no known rate limits for PLOS, though if you do 
#' hit something let us know.
#' 
#' **Crossref**: From time to time Crossref needs to impose rate limits 
#' to ensure that the free API is usable by all. Any rate limits that are in 
#' effect will be advertised in the `X-Rate-Limit-Limit` and
#' `X-Rate-Limit-Interval` HTTP headers. This boils down to: they allow X 
#' number of requests per some time period. The numbers can change so we 
#' can't give a rate limit that will always be in effect. If you're curious 
#' pass in `verbose = TRUE` to your function call, and you'll get headers 
#' that will display these rate limits. See also **Authentication**.
#' 
#' @section Authentication:
#' 
#' **BMC**: BMC is integrated into Springer Publishers now, 
#' and that API requires an API key.  Get your key by signing up at 
#' <https://dev.springer.com/>, then you'll get a key. Pass the key to a 
#' named parameter `key` to `bmcopts`. Or, save your key in your `.Renviron` 
#' file as `SPRINGER_KEY`, and we'll read it in for you, and you don't 
#' have to pass in anything.
#' 
#' **Scopus**: Scopus requires an API key to search their service. Go to 
#' <https://dev.elsevier.com/index.html>, register for an account, 
#' then when you're in your account, create an API key. Pass in as variable 
#' `key` to `scopusopts`, or store your key under the name 
#' `ELSEVIER_SCOPUS_KEY` as an environment variable in `.Renviron`, and 
#' we'll read it in for you. See [Startup] for help.
#'  
#' **Microsoft**: Get a key by creating an Azure account at 
#' <https://www.microsoft.com/cognitive-services/en-us/subscriptions>, 
#' then requesting a key for **Academic Knowledge API** within 
#' **Cognitive Services**. Store it as an environment variable in your 
#' `.Renviron` file - see [Startup] for help. Pass your 
#' API key into `maopts` as a named element in a list like 
#' `list(key = Sys.getenv('MICROSOFT_ACADEMIC_KEY'))`
#' 
#' **Crossref**: Crossref encourages requests with contact information 
#' (an email address) and will forward you to a dedicated API cluster 
#' for improved performance when you share your email address with them.
#' <https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service>
#' To pass your email address to Crossref via this client, store it 
#' as an environment variable in `.Renviron` like `crossref_email = name@example.com`
#' 
#' None needed for **PLOS**, **eLife**, **arxiv**, **biorxiv**, **Euro PMC**, or 
#' **Entrez** (though soon you will get better rate limtits with auth for Entrez)
#'
#' @section Feedback:
#' Let us know what you think at <https://github.com/ropensci/fulltext/issues>
#'
#' @importFrom xml2 read_html read_xml xml_find_first xml_find_all xml_text
#' xml_contents xml_attr xml_ns xml_children xml_name xml_ns_strip
#' @importFrom crul HttpClient
#' @importFrom httr HEAD GET POST upload_file content_type content write_disk
#' stop_for_status
#' @importFrom whisker whisker.render
#' @importFrom rentrez entrez_search entrez_fetch entrez_link entrez_summary
#' @importFrom rplos searchplos plos_fulltext
#' @importFrom rcrossref cr_works cr_ft_links
#' @importFrom aRxiv arxiv_search
#' @importFrom microdemic ma_abstract ma_evaluate
#' @importFrom storr storr_rds
#' @importFrom tibble as_tibble
#' @name fulltext-package
#' @aliases fulltext
#' @docType package
#' @author Scott Chamberlain <myrmecocystus@@gmail.com>
#' @author Will Pearse
#' @keywords package
NULL
