#' Scopus search
#' 
#' @export
#' @keywords internal
#' @param query (character) query terms, as a single character vector
#' @param count (integer/numeric) results to return: default: 25
#' @param start (integer/numeric) offset value, default: 0
#' @param type (character) type of search, default: search
#' @param search_type (character) search type, default: scopus
#' @param facets (list) facets, see 
#' https://dev.elsevier.com/tecdoc_api_facets.html for how to construct 
#' facet queries
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
#' }
scopus_search <- function(query = NULL, count = 25, start = 0, type = "search", 
                          search_type = "scopus", facets = NULL, key = NULL, ...) {
  key <- check_key_scopus(key)
  if (count > 25) stop("'count' for Scopus must be 25 or less", call. = FALSE)
  args <- ft_compact(list(query = query, count = count, start = start, 
    facets = facets))
  scopus_get(file.path(scopus_base(), "search/scopus"), args, key, ...)
}


#'@export 
scopus_search_loop <- function(query = NULL, count = 25, type = "search", 
                          search_type = "scopus", facets = NULL, key = NULL, ... ) {
  key <- check_key_scopus(key)
  lim <- if (count > 25) 25 else count
  #if (count > 25) stop("'count' for Scopus must be 25 or less", call. = FALSE)
  args <- ft_compact(list(query = query, count = lim, facets = facets))
  
  url <- file.path(scopus_base(), "search/scopus")
  out <- outfacet <- list()
  end <- FALSE
  i <- 0
  while (!end) {
    i <- i + 1
    res <- scopus_get(url, args, key, ...)
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

#'@export
scopus_abstract <- function(x, key, id_type = "doi", ...) {
  url <- file.path(scopus_base(), "abstract", id_type, x)
  json <- scopus_get(url, list(), key, ...)
  json$`abstracts-retrieval-response`$coredata$`dc:description`
}


#'@export
scopus_get <- function(url, args, key, ...) {
  cli <- crul::HttpClient$new(
    url = url, 
    headers = list(`X-ELS-APIKey` = key),
    opts = list(...)
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
