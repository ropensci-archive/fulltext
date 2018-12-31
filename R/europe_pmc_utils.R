#' Europe PMC utilities
#' 
#' @export
#' @keywords internal
#' @name eupmc
#' @param query (character) Search terms. Required. See Details
#' @param resulttype (character) The result type can either be idlist, 
#' core or lite. This parameter determines the fields returned by XML and 
#' JSON formats, but it has no effect on the DC format. See Details. 
#' @param synonym (boolean) Synonym searches are not made by default 
#' (Default: \code{FALSE}), however queries can be expanded using MeSH 
#' terminology and the UniProt synonyms list.  For example aspirin, a synonym 
#' would be acetylsalicylic acid;this could be included in 
#' the search by setting the parameter value to \code{TRUE}
#' @param per_page (integer) Number of records to return. Max: 1000. 
#' Default: 25
#' @param cursorMark (character) cursor string, default: `*`
#' @param sort (character) The default sort order is relevance. Specify the
#' sort field and sort order. This parameter provides "asc" or "desc" order 
#' for every single-valued field: P_PDATE_D, AUTH_FIRST, CITED etc. For 
#' example sorting by CITED in ascending order: CITED asc
#' @param ... curl options passed on to [crul::HttpClient]
#' @param id A single Europe PMC article identifier, begins with "PMC", 
#' followed by numbers, e.g.,  "PMC3257301"
#' 
#' @return `eupmc_search` returns a list with results. `eupmc_fields` returns
#' a data.frame. `eupmc_xml` returns an object of class `xml_document`
#' @section `query` parameter options:
#' 
#' - a keyword or combination of keywords (e.g. HPV virus).
#' - a phrase with enclosing speech marks (e.g. "human malaria").
#' - a fielded search (e.g. auth:stoehr). Available search fields are listed
#' in the Appendix 1 of the Reference Guide or can be retrieved using the 
#' fields module of the API.
#' - a specific publication (e.g. ext_id:781840 src:med) Specify ext_id as the 
#' article identifier, and src as the source database. List of the data sources
#' can be found on the help pages or in section 3 of the Reference Guide.
#' 
#' @section `resulttype` parameter options:
#' 
#' - idlist - returns a list of IDs and sources for the given search terms
#' - lite - returns key metadata for the given search terms; this is the 
#' default value if the parameter is unspecified.
#' - core - returns full metadata for a given publication ID; including 
#' abstract, full text links, and MeSH terms.
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
#' x <- eupmc_fields()
#' NROW(x)
#' head(x)
#' 
#' # get full text XML
#' eupmc_xml('PMC3257301')
#' }
eupmc_search <- function(query, resulttype = 'lite', synonym = FALSE, 
  per_page = 25, cursorMark = '*', ...) {

  is_it(per_page, "numeric")
  is_it(cursorMark, "character")
  is_it(synonym, "logical")
  args <- ft_compact(list(query = query, resultType = resulttype, 
    synonym = ft_as_log(synonym), pageSize = per_page, cursorMark = cursorMark,
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
  out <- cli$get(query = list(format = "json"))
  eupmc_errors(out)
  tt <- out$parse("UTF-8")
  jsonlite::fromJSON(tt)$searchTermList$searchTerms
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
