.full_text_urls <- function(doi) {
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
        ub <- 'http://journals.plos.org/plosclinicaltrials/article/asset?id=%s.XML'
        sprintf(ub, x)
      } else {
        ub <- 'http://journals.plos.org/%s/article/file?id=%s&type=manuscript'
        sprintf(ub, journal, x)
      }
    }
  }
  vapply(doi, makeurl, "", USE.NAMES = FALSE)
}

.plos_fulltext <- function(doi, disk, ...){
  url <- .full_text_urls(doi)
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  out <- cli$get(disk = disk)
  out$raise_for_status()
  if (!out$response_headers$`content-type` %in% c('application/xml', 
                                                  'text/xml')) {
    stop('content-type not one of "application/xml" or "text/xml"', 
         call. = FALSE)
  }
  return(out$content)
}
