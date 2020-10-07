# ft_links plugins --------------------------------------
plugin_links_entrez <- function(sources, ids, ...){
  if (any(grepl("entrez", sources))) {
    allids <- paste(paste0(ids, "[doi]"), collapse = " OR ")
    tmp <- rentrez::entrez_search(db = "pubmed", term = allids, config = c(...))
    if (length(tmp$ids) == length(ids)) {
      message("Only ", length(tmp$ids), " found for Entrez, proceeding...")
    }
    res <- rentrez::entrez_link(dbfrom = "pubmed", db = "", cmd = "llinks",
      id = tmp$ids, config = c(...))
    out <- lapply(res$linkouts, function(z) {
      rbl(lapply(z, function(w) {
        w <- unclass(w)
        tmp <- Map(function(a) {
          a[vapply(a, class, "") == "NULL"] <- NA_character_
          a
        }, w)
        df <- data.frame(tmp, stringsAsFactors = FALSE)
        stats::setNames(df, tolower(names(df)))
      }))
    })
    list(found = length(ft_compact(out)), ids = names(out), data = out)
  } else {
    emptylist()
  }
}

# ellipsis ignored, no http requests
plugin_links_plos <- function(sources, ids, ...){
  if (any(grepl("plos", sources))) {
    ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
    tmp <- as.list(stats::setNames(rplos::full_text_urls(ids), ids))
    tmp <- lapply(tmp, function(z) list(xml = z, pdf = sub("manuscript", "printable", z)))
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

plugin_links_crossref <- function(sources, ids, ...){
  if (any(grepl("crossref", sources))) {
    safe_crm_links <- function(x, type = "xml", ...) {
      tryCatch(crminer::crm_links(x, type, ...), error = function(e) NULL)
    }
    tmp <- ft_compact(lapply(ids, function(z) safe_crm_links(z, type = "all", ...)))
    out <- lapply(tmp, function(z) {
      rbl(lapply(z, function(w) {
        data.frame(url = w[[1]], doi = attr(w, "doi"), type = attr(w, "type"),
                   member = attr(w, "member") %||% "",
                   intended_application = attr(w, "intended_application") %||% "",
                   stringsAsFactors = FALSE)
      }))
    })
    out <- ft_compact(out)
    out <- stats::setNames(out, sapply(out, function(x) x$doi[1]))
    list(found = length(ft_compact(out)), ids = names(out),
         data = out)
  } else {
    emptylist()
  }
}

# ellipsis ignored, no http requests
plugin_links_bmc <- function(sources, ids, ...){
  if (any(grepl("bmc", sources))) {
    tmp <- stats::setNames(bmc_link(ids, ...), ids)
    # remove empty slots
    tmp <- Filter(function(z) !all(vapply(z, class, "") == "NULL"), tmp)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

bmc_link <- function(dois, ...) {
  pdfbase <- "https://%s/track/pdf/%s"
  lapply(dois, function(x) {
    res2 <- crul::HttpClient$new(paste0("https://doi.org/", x),
      opts = list(...))$head()
    if (!res2$success()) return(list(pdf = NULL))
    url <- crul::url_parse(res2$response_headers_all[[1]]$location)$domain
    list(pdf = sprintf(pdfbase, url, x))
  })
}

plugin_links_elife <- function(sources, ids, ...){
  if (any(grepl("elife", sources))) {
    tmp <- stats::setNames(elife_link(ids), ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

elife_link <- function(dois) {
  xmlbase <- "http://elifesciences.org/content/3/e%s.source.xml"
  pdfbase <- "http://elifesciences.org/content/elife/3/e%s.full.pdf"
  lapply(dois, function(x) {
    x <- strsplit(x, "eLife\\.")[[1]][2]
    list(xml = sprintf(xmlbase, x), pdf = sprintf(pdfbase, x))
  })
}

plugin_links_peerj <- function(sources, ids, ...){
  if (any(grepl("peerj", sources))) {
    tmp <- peerj_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

peerj_link <- function(dois) {
  xmlbase <- "https://peerj.com/articles/%s.xml"
  pdfbase <- "https://peerj.com/articles/%s.pdf"
  stats::setNames(lapply(dois, function(x) {
    x <- strsplit(x, "peerj\\.")[[1]][2]
    list(xml = sprintf(xmlbase, x), pdf = sprintf(pdfbase, x))
  }), dois)
}

plugin_links_frontiersin <- function(sources, ids, ...){
  if (any(grepl("frontiersin", sources))) {
    tmp <- frontiersin_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

frontiersin_link <- function(dois) {
  xmlbase <- "http://journal.frontiersin.org/article/%s/xml/nlm"
  pdfbase <- "http://journal.frontiersin.org/article/%s/pdf"
  stats::setNames(lapply(dois, function(x) {
    list(xml = sprintf(xmlbase, x), pdf = sprintf(pdfbase, x))
  }), dois)
}

plugin_links_copernicus <- function(sources, ids, ...){
  if (any(grepl("copernicus", sources))) {
    tmp <- copernicus_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

copernicus_link <- function(dois) {
  xmlbase <- "http://www.ann-geophys.net/%s/%s/%s/%s.xml"
  pdfbase <- "http://www.ann-geophys.net/%s/%s/%s/%s.pdf"
  stats::setNames(lapply(dois, function(x) {
    x <- strsplit(x, "/")[[1]][2]
    pcs <- strsplit(x, "-")[[1]]
    list(xml = sprintf(xmlbase, pcs[2], pcs[3], pcs[4], x),
         pdf = sprintf(pdfbase, pcs[2], pcs[3], pcs[4], x))
  }), dois)
}

plugin_links_cogent <- function(sources, ids, ...){
  if (any(grepl("cogent", sources))) {
    tmp <- cogent_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

cogent_link <- function(dois) {
  xmlbase <- "http://cogentoa.tandfonline.com/doi/xml/"
  pdfbase <- "http://cogentoa.tandfonline.com/doi/pdf/"
  stats::setNames(lapply(dois, function(x) {
    list(xml = paste0(xmlbase, x), pdf = paste0(pdfbase, x))
  }), dois)
}

plugin_links_rsoc <- function(sources, ids, ...) {
  if (any(grepl("rsoc", sources))) {
    tmp <- rsoc_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

rsoc_link <- function(dois) {
  pdfbase <- "https://royalsocietypublishing.org/doi/pdf/"
  stats::setNames(lapply(dois, function(x) {
    list(pdf = paste0(pdfbase, x))
  }), dois)
}

plugin_links_rsoc <- function(sources, ids, ...) {
  if (any(grepl("rsoc", sources))) {
    tmp <- rsoc_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

rsoc_link <- function(dois) {
  pdfbase <- "https://royalsocietypublishing.org/doi/pdf/"
  stats::setNames(lapply(dois, function(x) {
    list(pdf = paste0(pdfbase, x))
  }), dois)
}

plugin_links_cdc <- function(sources, ids, ...) {
  if (any(grepl("cdc", sources))) {
    tmp <- cdc_link(ids)
    list(found = length(tmp), ids = names(tmp), data = tmp)
  } else {
    emptylist()
  }
}

cdc_link <- function(dois) {
  stats::setNames(lapply(dois, function(x) {
    tt <- tcat(ftd_doi(x))
    if (inherits(tt, c("error", "warning"))) return(list(pdf = NA_character_))
    if (is.na(tt$url)) return(list(pdf = NA_character_))  
    list(pdf = tt$url)
  }), dois)
}

# empty list helper
emptylist <- function() {
  list(found = NULL, ids = NULL,  data = NULL)
}
