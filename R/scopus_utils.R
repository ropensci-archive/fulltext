#' Scopus search
#' 
#' @name scopus_search
#' @keywords internal
#' @param query (character) query terms, as a single character vector
#' @param count (integer/numeric) results to return: default: 25
#' @param start (integer/numeric) offset value, default: 0
#' @param type (character) type of search, default: search
#' @param search_type (character) search type, default: scopus
#' @param facets (list) facets, see 
#' https://dev.elsevier.com/tecdoc_api_facets.html for how to construct 
#' facet queries
#' @param view the fields to return, see 
#' https://dev.elsevier.com/guides/ScopusSearchViews.htm 
#' @param date Represents the date range associated with the search, 
#' with the lowest granularity being year. e.g. 2002-2007
#' @param sort Represents the sort field name and order. A plus in front of 
#' the sort field name indicates ascending order, a minus indicates 
#' descending order. If sort order is not specified (i.e. no + or -) then 
#' the order defaults to ascending (ASC). Up to three fields can be 
#' specified, each delimited by a comma. The precedence is determined by 
#' their order (i.e. first is primary, second is secondary, and 
#' third is tertiary). . e.g., "overDate,-title"
#' @param content filter specific categories of content that should be 
#' searched/returned. one of: core, dummy, all (default)
#' @param subj the subject area code associated with the content category 
#' desired. Note that these subject code mapping vary based upon the 
#' environment in which the request is executed. See Details for choices.
#' @param key (character) api key. get a key at 
#' <https://dev.elsevier.com/index.html>
#' @param ... curl options passed on to [crul::HttpClient]
#' 
#' @details Rate limits for search are 20,000 per every 7 days. You likely
#' won't make that many requests in 7 days, but if you do e.g., make 20K in
#' 5 days, then you have to wait 2 days for the clock to reset, than you'll 
#' be able to make 20K again. 
#' 
#' See <https://dev.elsevier.com/api_key_settings.html> for rate 
#' limit information.
#' 
#' See <https://dev.elsevier.com/tips/ScopusSearchTips.htm> for help/tips
#' on searching
#' 
#' @section subj choices include:
#' 
#' - AGRI: Agricultural and Biological Sciences
#' - ARTS: Arts and Humanities
#' - BIOC: Biochemistry, Genetics and Molecular Biology
#' - BUSI: Business, Management and Accounting
#' - CENG: Chemical Engineering
#' - CHEM: Chemistry
#' - COMP: Computer Science
#' - DECI: Decision Sciences
#' - DENT: Dentistry
#' - EART: Earth and Planetary Sciences
#' - ECON: Economics, Econometrics and Finance
#' - ENER: Energy
#' - ENGI: Engineering
#' - ENVI: Environmental Science
#' - HEAL: Health Professions
#' - IMMU: Immunology and Microbiology
#' - MATE: Materials Science
#' - MATH: Mathematics
#' - MEDI: Medicine
#' - NEUR: Neuroscience
#' - NURS: Nursing
#' - PHAR: Pharmacology, Toxicology and Pharmaceutics
#' - PHYS: Physics and Astronomy
#' - PSYC: Psychology
#' - SOCI: Social Sciences
#' - VETE: Veterinary
#' - MULT: Multidisciplinary
#' 
#' @examples \dontrun{
#' res <- scopus_search(query = "ecology")
#' res
#' 
#' #scopus_search(query = x, type = "abstract")
#' 
#' # looping through
#' res <- scopus_search_loop(query = "ecology community elk cow")
#' 
#' # using facets
#' ## scopus_search
#' res <- scopus_search(query = "ecology", facets = "subjarea(count=5)")
#' res
#' res$`search-results`$link
#' res$`search-results`$entry
#' res$`search-results`$facet
#' 
#' ## more examples
#' x <- scopus_search(query = "ecology", facets = "language(count=4)", count = 1)
#' x$`search-results`$facet
#' x <- scopus_search(query = "ecology", facets = "pubyear(count=3);doctype();language(count=4)")
#' x$`search-results`$facet
#' 
#' ## scopus_search_loop
#' res <- scopus_search_loop(query = "ecology", facets = "subjarea(count=5)", count = 200)
#' res$found
#' head(res$results)
#' res$facets
#' 
#' # sort
#' x <- scopus_search(query = "ecology", sort = "-title")
#' }

#' @export
scopus_search <- function(query = NULL, count = 25, start = 0, type = "search", 
  search_type = "scopus", facets = NULL, view = NULL, date = NULL, 
  sort = NULL, content = NULL, subj = NULL, key = NULL, ...) {

  key <- check_key_scopus(key)
  if (count > 25) stop("'count' for Scopus must be 25 or less", call. = FALSE)
  args <- ft_compact(list(query = query, count = count, start = start, 
    facets = facets, view = view, date = date, sort = sort, 
    content = content, subj = subj))
  scopus_get(file.path(scopus_base(), "search/scopus"), args, key, list(), ...)
}

#' @export
#' @rdname scopus_search
scopus_search_loop <- function(query = NULL, count = 25, start = 0, type = "search", 
  search_type = "scopus", facets = NULL, view = NULL, date = NULL, 
  sort = NULL, content = NULL, subj = NULL, key = NULL, ... ) {

  key <- check_key_scopus(key)
  lim <- if (count > 25) 25 else count
  args <- ft_compact(list(query = query, count = lim, start = start, 
    facets = facets, view = view, date = date, sort = sort, 
    content = content, subj = subj))
  
  url <- file.path(scopus_base(), "search/scopus")
  out <- outfacet <- list()
  end <- FALSE
  i <- 0
  while (!end) {
    i <- i + 1
    res <- scopus_get(url, args, key, list(), ...)
    tot <- as.numeric(res$`search-results`$`opensearch:totalResults`)
    if (tot < 1) {
      end <- TRUE
      out[[i]] <- data.frame(NULL)
    } else {
      out[[i]] <- res$`search-results`$entry
      links <- res$`search-results`$link
      outfacet <- res$`search-results`$facet
      # next url to use
      url <- links[links$`@ref` == "next", '@href']
      # and set args to an empty list()
      args <- list()
      if (NROW(rbl(out)) >= min(c(count, tot))) end <- TRUE
    }
  }
  list(results = rbl(out), facets = outfacet, found = tot)
}

scopus_abstract <- function(x, key, id_type = "doi", curlopts = list()) {
  url <- file.path(scopus_base(), "abstract", id_type, x)
  json <- scopus_get(url, list(), key, curlopts)
  json$`abstracts-retrieval-response`$coredata$`dc:description`
}

scopus_get <- function(url, args, key, curlopts, ...) {
  cli <- crul::HttpClient$new(
    url = url, 
    headers = list(`X-ELS-APIKey` = key),
    opts = c(curlopts, list(...))
  )
  res <- cli$get(query = args)
  scopus_error_handle(res)
  txt <- res$parse("UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

scopus_base <- function() "https://api.elsevier.com/content"

check_key_scopus <- function(x) {
  tmp <- if (is.null(x)) {
    Sys.getenv("ELSEVIER_SCOPUS_KEY", "")
  } else {
    x
  }
  if (tmp == "") {
    stop("you need an API key for the Scopus API",
         call. = FALSE)
  }
  return(tmp)
}

scopus_error_handle <- function(x) {
  if (x$status_code > 201) {
    txt <- x$parse("UTF-8")
    json <- jsonlite::fromJSON(txt, flatten = TRUE)  
    mssg <- json$`service-error`$status$statusText
    if (is.null(mssg)) x$raise_for_status()
    stop(mssg, call. = FALSE)
  }
  if ('www-authenticate' %in% names(x$response_headers)) {
    warning(sprintf("  for %s ", x), x$response_headers$`www-authenticate`, 
      call. = FALSE)
  } 
}
