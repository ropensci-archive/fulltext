#' Scopus search
#' 
#' @export
#' @keywords internal
#' @param query query terms, as a single character vector
#' @param count results to return: default: 25
#' @param start offset value, default: 0
#' @param type type of search, default: search
#' @param search_type search type, default: scopus
#' @param key api key
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' res <- scopus_search(query = "ecology")
#' res
#' 
#' #scopus_search(query = x, type = "abstract")
#' }
scopus_search <- function(query = NULL, count = 25, start = 0, type = "search", 
                          search_type = "scopus", key = NULL, ... ) {
  key <- check_key_scopus(key)
  if (count > 25) stop("'count' for Scopus must be 25 or less", call. = FALSE)
  args <- ft_compact(list(query = query, apiKey = key, count = count, start = start))
  res <- httr::GET(file.path(scopus_base(), "search/scopus"), query = args, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

scopus_base <- function() "http://api.elsevier.com/content"

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

scopus_abstract <- function(x, key, id_type = "doi", ...) {
  args <- ft_compact(list(apiKey = key))
  url <- file.path(scopus_base(), "abstract", id_type, x)
  res <- httr::GET(url, query = args, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt, flatten = TRUE)
  json$`abstracts-retrieval-response`$coredata$`dc:description`
}
