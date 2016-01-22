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
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @param id A single Europe PMC article identifier, begins with "PMC", followed by numbers, e.g., 
#' "PMC3257301"
#' 
#' @return \code{eupmc_search} returns a list with results. \code{eupmc_fields} returns 
#' a data.frame. \code{eupmc_xml} returns an object of class \code{xml_document}
#' @details resulttype parameter options:
#' \itemize{
#'  \item idlist - returns a list of IDs and sources for the given search terms
#'  \item lite - returns key metadata for the given search terms; this is the default 
#'  value if the parameter is unspecified.
#'  \item core - returns full metadata for a given publication ID; including abstract, 
#'  full text links, and MeSH terms.
#' }
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
eupmc_search <- function(query, resulttype = NULL, synonym = FALSE, per_page = 25, page = 1, ...) {
  is_it(per_page, "numeric")
  is_it(page, "numeric")
  is_it(synonym, "logical")
  args <- ft_compact(list(query = query, resulttype = resulttype, synonym = ft_as_log(synonym), 
                          pageSize = per_page, page = page, format = "json"))
  newargs <- paste(names(args), args, sep = "=", collapse = "&")
  out <- GET(URLencode(paste0(eupmc_base(), "search/", newargs)), ...)
  eupmc_errors(out)
  tt <- content(out, "text")
  jsonlite::fromJSON(tt)
}

#' @export
#' @keywords internal
#' @rdname eupmc
eupmc_fields <- function(...) {
  out <- GET(URLencode(paste0(eupmc_base(), "fields")), ...)
  eupmc_errors(out)
  tt <- content(out, "text")
  tmp <- xml2::as_list(xml2::read_xml(tt))
  out <- lapply(tmp$searchTermList, function(z) {
    setNames(rbind.data.frame(z), c('term', 'datasets'))
  })
  do.call("rbind.data.frame", unname(out))
}

#' @export
#' @keywords internal
#' @rdname eupmc
eupmc_xml <- function(id, ...) {
  out <- GET(URLencode(paste0(eupmc_base(), id, "/fullTextXML")), ...)
  eupmc_errors(out)
  tt <- content(out, "text")
  xml2::read_xml(tt)
}

# helpers
eupmc_base <- function() "http://www.ebi.ac.uk/europepmc/webservices/rest/"

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
  if (!is(x, y)) stop(sprintf("%s must be %s", deparse(substitute(x)), y), call. = FALSE)  
}
