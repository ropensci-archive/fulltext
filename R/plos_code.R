.full_text_urls <- function(doi, type) {
  makeurl <- function(x) {
    if (grepl("annotation", x)) {
      NA_character_
    } else {
      doijournal <- strsplit(x, "\\.")[[1]][[3]]
      journal <- switch(doijournal,
                        pone = 'plosone',
                        pbio = 'plosbiology',
                        pmed = 'plosmedicine',
                        pgen = 'plosgenetics',
                        pcbi = 'ploscompbiol',
                        ppat = 'plospathogens',
                        pntd = 'plosntds',
                        pctr = 'plosclinicaltrials')
      if ("plosclinicaltrials" == journal) {
        ub <- 'https://journals.plos.org/plosclinicaltrials/article/asset?id=%s.XML'
        sprintf(ub, x)
      } else {
        ub <- 'https://journals.plos.org/%s/article/file?id=%s&type=%s'
        sprintf(ub, journal, x, switch(type, xml="manuscript", pdf="printable"))
      }
    }
  }
  vapply(doi, makeurl, "", USE.NAMES = FALSE)
}

xml_ctypes <- c('application/xml', 'text/xml')
pdf_ctypes <- 'application/pdf'

.plos_fulltext <- function(doi, disk, type, ...){
  url <- .full_text_urls(doi, type)
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  out <- cli$get(disk = disk)
  out$raise_for_status()
  if (
    !out$response_headers$`content-type` %in%
    switch(type, xml=xml_ctypes, pdf=pdf_ctypes)
  ) {
    stop(paste0('content-type not one of ',
      paste0(switch(type, xml=xml_ctypes, pdf=pdf_ctypes),
        collapse = " or ")),
         call. = FALSE)
  }
  return(out$content)
}
