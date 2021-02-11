#' @title Fulltext search and retrieval of scholarly texts.
#'
#' @description fulltext is a single interface to many sources of scholarly
#' texts. In practice, this means only ones that are legally useable. We will
#' support sources that require authentication on a case by case basis - that
#' is, if more than just a few people will use it, and it's not too
#' burdensome to include, then we can include that source.
#'
#' @section Manual:
#' See https://books.ropensci.org/fulltext/ for a longer form
#' manual for using \pkg{fulltext}.
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
#' - extract certain article sections (e.g., authors) - moved to \pkg{pubchunks}
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
#' https://dev.elsevier.com/api_key_settings.html for rate
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
#' **Semantic Scholar**: Not documented in their docs, and no response
#' headers given. At time of this writing (2020-07-01) the rate limit is:
#' 100 requests per 5-minutes per IP address. or 20 requests per min. Note
#' that this rate limit may change.
#'
#' @section Authentication:
#'
#' **BMC**: BMC is integrated into Springer Publishers now,
#' and that API requires an API key.  Get your key by signing up at
#' https://dev.springer.com/, then you'll get a key. Pass the key to a
#' named parameter `key` to `bmcopts`. Or, save your key in your `.Renviron`
#' file as `SPRINGER_KEY`, and we'll read it in for you, and you don't
#' have to pass in anything.
#'
#' **Scopus**: Scopus requires two things: an API key and your institution must
#' have access. For the API key, go to https://dev.elsevier.com/index.html,
#' register for an account, then when you're in your account, create an API key.
#' Pass in as variable `key` to `scopusopts`, or store your key under the name
#' `ELSEVIER_SCOPUS_KEY` as an environment variable in `.Renviron`, and
#' we'll read it in for you. See [Startup] for help. For the institution access
#' go to a browser and see if you have access to the journal(s) you want.
#' If you don't have access in a browser you probably won't have access via
#' this package. If you aren't physically at your institution you will likely
#' need to be on a VPN or similar and eventually require correct proxy settings,
#' so that your IP address is in the range
#' that the two publishers are accepting for that institution.
#' It might be, that the API access seems to work even while
#' in the wrong IP range or have wrong proxy settings,
#' but you are not able to see the abstracts, they will be empty.
#' By using the currect curl options into the calls to ft_search or ft_abstracts even
#' the most complex proxy including authentication should work. As an example:
#'
#' \preformatted{
#' opts <- list(key="your-scopus-key")
#' ft_abstract(x = dois, from = "scopus", scopusopts = opts,
#'   proxy="proxy-ip-address",
#'   proxyport=your-proxy-port,
#'   proxyuserpwd="username:password", # often the same as your windows login
#'   proxyauth=8) # ntlm - authentication
#' }
#'
#' **Elsevier/ScienceDirect**: Elsevier ScienceDirect requires two things: an
#' API key and your institution must have access. For the API key,
#' go to https://dev.elsevier.com/index.html,
#' register for an account, then when you're in your account, create an API key
#' that is allowed to access the TDM API (must accept their TDM policy).
#' Pass in as variable `key` to `elsevieropts`/`sciencedirectopts`, or store
#' your key under the name `ELSEVIER_TDM_KEY` as an environment variable in
#' `.Renviron`, and we'll read it in for you. See [Startup] for help. For
#' the institution access
#' go to a browser and see if you have access to the journal(s) you want.
#' If you don't have access in a browser you probably won't have access via
#' this package. If you aren't physically at your institution you will likely
#' need to be on a VPN or similar so that your IP address is in the range
#' that the publisher is accepting for that institution.
#'
#' **Wiley**: Replacing Crossref TDM service as of February 2021, Wiley
#' now requires you get a Wiley TDM key. Get one at
#' https://onlinelibrary.wiley.com/library-info/resources/text-and-datamining
#' Pass in as variable `key` to `wileyopts`, or preferably store
#' your key under the name `WILEY_TDM_KEY` as an environment variable in
#' `.Renviron`, and we'll read it in for you. See [Startup] for help. Some
#' notes about Wiley's TDM service:
#'
#' - They always respond initially with a redirect to a server dedicated to
#' the serving of binary resources - fulltext takes care of this
#' - Wiley uses rate-limiting: no more than 3 requests per second. you
#' may get 429 errors if making too requests too rapidly
#'
#' **Microsoft**: Get a key by creating an Azure account
#' then request a key for **Academic Knowledge API** within
#' **Cognitive Services**. Store it as an environment variable in your
#' `.Renviron` file - see [Startup] for help. Pass your
#' API key into `maopts` as a named element in a list like
#' `list(key = Sys.getenv('MICROSOFT_ACADEMIC_KEY'))`
#'
#' **Crossref**: Crossref encourages requests with contact information
#' (an email address) and will forward you to a dedicated API cluster
#' for improved performance when you share your email address with them. This 
#' is called the "Polite Pool". 
#' https://github.com/CrossRef/rest-api-doc#good-manners--more-reliable-service
#' To pass your email address to Crossref via this client, store it
#' as an environment variable in `.Renviron` like
#' `crossref_email=name@example.com`, or `CROSSREF_EMAIL=name@example.com`.
#' Save the file and restart your R session. To stop sharing your email when
#' using rcrossref simply delete it from your `.Renviron` file OR to temporarily
#' not use your email unset it for the session
#' like `Sys.unsetenv('crossref_email')`. To be sure your in the polite pool
#' use curl verbose by e.g., `ft_cr_links(doi = "10.5555/515151", verbose = TRUE)`
#' 
#' **Crossref TDM**: TDM = "Text and Data Mining". This used to apply to just
#' two publishers - Wiley and Elsevier - This service officially shut down at
#' the end of 2020. For Elsevier, see the
#' "Elsevier/ScienceDirect" section above. For Wiley, see the "Wiley" section
#' above.
#'
#' **Entrez**: NCBI limits users to making only 3 requests per second. But, users
#' who register for an API key are able to make up to ten requests per second.
#' Getting a key is simple; register for a "my ncbi" account then click on a
#' button in the account settings page. Once you have an API key, you can pass it
#' as the argument `api_key` to `entrezopts` in both [ft_get()] and [ft_search()].
#' However, we advise you use environment variables instead as they are more secure.
#' To do that you can set an environment variable for the current R session like
#' `Sys.setenv(ENTREZ_KEY="yourkey")` OR better yet set it in your `.Renviron`
#' or equivalent file with an entry like `ENTREZ_KEY=yourkey` so that it is
#' used across R sessions.
#'
#' No authentication needed for **PLOS**, **eLife**, **arxiv**, **biorxiv**,
#' **Euro PMC**
#'
#' Open an issue if you run into trouble with authentication.
#'
#' @section Feedback:
#' Let us know what you think at https://github.com/ropensci/fulltext/issues
#'
#' @importFrom xml2 read_html read_xml xml_find_first xml_find_all xml_text
#' xml_contents xml_attr xml_ns xml_children xml_name xml_ns_strip
#' @importFrom crul HttpClient Async
#' @importFrom rentrez entrez_search entrez_fetch entrez_link entrez_summary
#' @importFrom rplos searchplos plos_fulltext
#' @importFrom rcrossref cr_works cr_ft_links
#' @importFrom aRxiv arxiv_search
#' @importFrom microdemic ma_abstract ma_evaluate
#' @importFrom storr storr_rds
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @importFrom digest digest
#' @name fulltext-package
#' @aliases fulltext
#' @docType package
#' @author Scott Chamberlain
#' @author Will Pearse
#' @author Helge Knüttel
#' @keywords package
NULL
