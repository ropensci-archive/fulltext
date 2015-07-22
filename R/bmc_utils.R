# bmc functions

## search for bmc papers
# bmc_search(terms = 'ecology')
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

## get xml
bmc_xml <- function(obj=NULL, uris=NULL, dir=NULL, raw=FALSE, ...) {
  if (!is.null(obj)) { 
    stopifnot(is(obj, "bmc"))
    toget <- obj@ids
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
      tt <- content(res, as = "text")
      
      if (raw) { 
        tt 
      } else {
        xml <- tryCatch(xmlParse(tt), error = function(e) e, silent = TRUE)
        if (is(xml, 'simpleError')) {
          message(sprintf('%s is not valid xml', x))
        } else {
          if (!is.null(dir)) {
            filedir <- paste0(dir, strextract(x, "[0-9].+")[[1]], collapse = '')
            saveXML(xml, file = filedir)
          } else { 
            return( xml ) 
          }
        }
      }
      
    }
  }
  
  lapply(uris, getxml, ...)
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
