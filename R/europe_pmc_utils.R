#' Europe PMC utilities
#' 
#' @export
#' @keywords internal
#' @name eupmc
#' @param query (character) Search terms. Required.
#' @param resulttype (character) The result type can either be idlist, core or lite. 
#' This parameter determines the fields returned by XML and JSON formats, but it has 
#' no effect on the DC format. See Details. 
#' @param synonym (boolean) Synonym searches are not made by default (Default: \code{FALSE}), 
#' however queries can be expanded using MeSH terminology and the UniProt synonyms list. 
#' For example aspirin, a synonym would be acetylsalicylic acid;this could be included in 
#' the search by setting the parameter value to \code{TRUE}
#' @param per_page (integer) Number of records to return. Max: 1000. Default: 25
#' @param page Page (integer) Page numbers start at 1. Default: 1
#' @param ... curl options passed on to [crul::HttpClient]
#' @param id A single Europe PMC article identifier, begins with "PMC", followed by 
#' numbers, e.g.,  "PMC3257301"
#' 
#' @return `eupmc_search` returns a list with results. `eupmc_fields` returns 
#' a data.frame. `eupmc_xml` returns an object of class `xml_document`
#' @details resulttype parameter options:
#' 
#' - idlist - returns a list of IDs and sources for the given search terms
#' - lite - returns key metadata for the given search terms; this is the default 
#'  value if the parameter is unspecified.
#' - core - returns full metadata for a given publication ID; including abstract, 
#'  full text links, and MeSH terms.
#' 
#' @references https://europepmc.org/RestfulWebService
#' 
#' @examples \dontrun{
#' # search
#' eupmc_search(query = 'ecology')
#' eupmc_search(query = 'human malaria')
#' eupmc_search(query = '"human malaria"')
#' eupmc_search(query = 'auth:stoehr')
#' eupmc_search(query = 'journal:pnas')
#' eupmc_search(query = 'journal:pnas')
#' eupmc_search(query = 'ext_id:781840 src:med')
#' eupmc_search(query = 'ext_id:IND43783977 src:agr')
#' 
#' # list indexed search fields
#' head(eupmc_fields())
#' 
#' # get full text XML
#' eupmc_xml('PMC3257301')
#' }
eupmc_search <- function(query, resulttype = NULL, synonym = FALSE, per_page = 25, 
  page = 1, ...) {

  is_it(per_page, "numeric")
  is_it(page, "numeric")
  is_it(synonym, "logical")
  args <- ft_compact(list(query = query, resulttype = resulttype, 
    synonym = ft_as_log(synonym), pageSize = per_page, page = page, 
    format = "json"))
  newargs <- paste(names(args), args, sep = "=", collapse = "&")
  cli <- crul::HttpClient$new(
    url = utils::URLencode(paste0(eupmc_base(), "search/", newargs)),
    opts = list(...)
  )
  out <- cli$get()
  eupmc_errors(out)
  tt <- out$parse("UTF-8")
  jsonlite::fromJSON(tt)
}

#' @export
#' @keywords internal
#' @rdname eupmc
eupmc_fields <- function(...) {
  cli <- crul::HttpClient$new(
    url = utils::URLencode(paste0(eupmc_base(), "fields")),
    opts = list(...))
  out <- cli$get()
  eupmc_errors(out)
  tt <- out$parse("UTF-8")
  tmp <- xml2::as_list(xml2::read_xml(tt))
  data.frame(terms = unname(unlist(lapply(tmp$searchTermList, "[[", "term"))), 
    stringsAsFactors=FALSE)
}

#' @export
#' @keywords internal
#' @rdname eupmc
eupmc_xml <- function(id, ...) {
  cli <- crul::HttpClient$new(
    url = utils::URLencode(paste0(eupmc_base(), id, "/fullTextXML")), 
    opts = list(...))
  out <- cli$get()
  eupmc_errors(out)
  tt <- out$parse("UTF-8")
  xml2::read_xml(tt)
}

# helpers
eupmc_base <- function() "https://www.ebi.ac.uk/europepmc/webservices/rest/"

ft_as_log <- function(x) {
  stopifnot(is.logical(x))
  if (x) 'true' else 'false'
}

eupmc_errors <- function(x) {
  if (x$status_code > 201) {
    stop(x$status_code, " - Bad request", call. = FALSE)
  }
}

is_it <- function(x, y) {
  if (!inherits(x, y)) stop(sprintf("%s must be %s", deparse(substitute(x)), y), call. = FALSE)  
}
