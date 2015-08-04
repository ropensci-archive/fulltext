#' Search for gene sequences available for a species from NCBI.
#'
#' @export
#' @keywords internal
#' @param terms Search terms.
#' @param limit Number of records to return. Max 25. Default 10.
#' @param page Page number. Only applies if more than 25 results.
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @return A list.
#' @examples \dontrun{
#' bmc_search(terms = 'ecology')
#' bmc_search(terms = 'fire', limit=3)
#' bmc_search(terms = 'fire', limit=2, page=1)
#' bmc_search(terms = 'fire', limit=2, page=2)
#'
#' (out <- bmc_search(terms = 'fire', limit=5))
#'
#' # Search, then get full text
#' out <- bmc_search(terms = 'ecology')
#' out$urls # you could use these to go to the website
#' out$ids # used to construct download urls in bmc_xml
#' browseURL(out$urls[1])
#'
#' # curl debugging help
#' library('httr')
#' bmc_search(terms = 'ecology', config=verbose())
#' }
bmc_search <- function(terms, limit=10, page=1, ...) {
  url = 'http://www.biomedcentral.com/search/results'
  args <- ft_compact(list(terms = terms, itemsPerPage = limit, page = page, format = 'json'))
  out <- GET(url, query = args, ...)
  stop_for_status(out)
  tt <- content(out)
  urls <- vapply(tt$entries, function(z) z[['articleFullUrl']], "character")
  ids <- lapply(tt$entries, function(z) list(arxId = z[['arxId']], url = z[['articleFullUrl']]))
  list(urls = urls, ids = ids, results = tt)
}

#' Download full text xml of a BMC paper.
#'
#' @rdname bmc
#' @export
#' @keywords internal
#' @param obj (optional) An object of class bmc, from a call to \code{bmc_search}
#' @param uris (optional) A uri to a xml file of a BMC paper
#' @param dir (optional) A directory to save to. The file extension is forced to
#' .xml, and the file name will be
#' @param raw (logical) If TRUE, returns raw text, but if FALSE, parsed XML. Default: FALSE
#' @param ... Futher args passed on to \code{\link[httr]{GET}} for debugging curl calls.
#' @examples \dontrun{
#' uri = 'http://www.biomedcentral.com/content/download/xml/1471-2393-14-71.xml'
#' uri = 'http://www.springerplus.com/content/download/xml/2193-1801-3-7.xml'
#' uri = 'http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml'
#' bmc_xml(uris=uri)
#' bmc_xml(uris=uri, dir='~/')
#' bmc_xml(uris=uri, verbose())
#'
#' # from using bmc_search
#' out <- bmc_search(terms = 'science', limit=5)
#' dat <- bmc_xml(out)
#' length(dat)
#' dat <- Filter(Negate(is.null), dat)
#' length(dat)
#' dat
#'
#' # curl debugging, and other parameters passed to httr::GET()
#' uri = 'http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml'
#' res <- bmc_xml(uris=uri, config=verbose())
#' # res <- bmc_xml(uris=uri, config=timeout(0.1))
#'
#' uri1 = 'http://www.biomedcentral.com/content/download/xml/1471-2393-14-71.xml'
#' uri2 = 'http://www.springerplus.com/content/download/xml/2193-1801-3-7.xml'
#' uri3 = 'http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml'
#' res <- bmc_xml(uris=list(uri1, uri2, uri3), config=progress())
#' }
bmc_xml <- function(obj=NULL, uris=NULL, dir=NULL, raw=FALSE, ...) {
  if (!is.null(obj)) {
    # stopifnot(is(obj, "bmc"))
    toget <- obj$ids
    # construct download url
    uris <- vapply(toget, function(w){
      url <- gsub('[0-9].+', '', w['url'])
      paste0(url, 'download/xml/', w['arxId'], '.xml')
    }, "")
  }

  getxml <- function(x, ...){
    res <- GET(x, ...)
    if (!res$status_code == 200) {
      message(sprintf('%s not found, or xml not available', x))
    } else {
      tt <- content(res, as = "text", encoding = "UTF-8")

      if (raw) {
        tt
      } else {
        xml <- tryCatch(xml2::read_xml(tt), error = function(e) e, silent = TRUE)
        if (is(xml, 'simpleError')) {
          message(sprintf('%s is not valid xml', x))
        } else {
          if (!is.null(dir)) {
            filedir <- path.expand(paste0(dir, strextract(x, "[0-9].+")[[1]], collapse = ''))
            xml2::write_xml(xml, filedir)
          } else {
            return( xml )
          }
        }
      }

    }
  }

  lapply(uris, getxml, ...)
}
