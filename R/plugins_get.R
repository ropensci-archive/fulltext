# get plugins --------------------------------------

# cache helper --------------------------------------
construct_paths <- function(co, x){
  if (!co$cache) {
    list(backend = NULL, 
         path = "session",
         data = x)
  } else {
    list(backend = co$backend,
         path = cache_save(obj = x, backend = co$backend, path = co$path),
         data = NULL)
  }
}

## plugin generator
plugin_get_generator <- function(srce, fun) {
  function(sources, ids, opts, path = NULL, ...) {
    if (any(grepl("plos", sources))) {
      ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
    }
    
    callopts <- list(...)
    if (any(grepl(eval(srce), sources))) {
      if (!any(grepl("arxiv", sources))) check_dois(ids)
      if (any(grepl("plos", sources))) {
        opts$doi <- ids
      } else if (any(sources %in% c("entrez", "crossref"))) {
        opts$ids <- ids
      } else {
        opts$dois <- ids
      }
      if (any(sources %in% c("arxiv", "biorxiv"))) opts$basepath <- path
      opts <- c(opts, callopts)
      out <- do.call(fun, opts)
      names(out) <- ids
      attr(out, "format") <- "xml"
      dat <- if (any(sources %in% c("arxiv", "biorxiv"))) {
        pprint_cache(out)
      } else {
        construct_paths(cache_options_get(), out)
      }
      list(found = length(out), dois = names(out), data = dat, opts = opts)
    } else {
      list(found = NULL, dois = NULL, data = NULL, opts = opts)
    }
  }
}

pprint_cache <- function(x) {
  list(backend = NULL, path = x, data = NULL) 
}

## make plugins
plugin_get_plos <- plugin_get_generator("plos", plos_fulltext)
plugin_get_bmc <- plugin_get_generator("bmc", bmc_ft)
plugin_get_elife <- plugin_get_generator("elife", elife_paper)
plugin_get_peerj <- plugin_get_generator("peerj", peerj_ft)
plugin_get_frontiersin <- plugin_get_generator("frontiersin", frontiersin_ft)
plugin_get_pensoft <- plugin_get_generator("pensoft", pensoft_ft)
plugin_get_copernicus <- plugin_get_generator("copernicus", copernicus_ft)
plugin_get_cogent <- plugin_get_generator("cogent", cogent_ft)
plugin_get_crossref <- plugin_get_generator("crossref", cr_ft_xml)
plugin_get_entrez <- plugin_get_generator("entrez", entrez_get)
plugin_get_biorxiv <- plugin_get_generator("biorxiv", biorxiv_ft)
plugin_get_arxiv <- plugin_get_generator("arxiv", arxiv_ft)

## getters - could stand to make closure for the below as well, FIXME
entrez_get <- function(ids, ...){
  res <- rentrez::entrez_search(db = "pmc", term = paste0(sprintf('%s[doi]', ids), collapse = "|"), ...)
  if (length(res$ids) == 0) {
    res <- rentrez::entrez_search(db = "pubmed", term = paste0(sprintf('%s[doi]', ids), collapse = "|"), ...)
    vapply(res$ids, function(z) rentrez::entrez_fetch(db = 'pubmed', id = z, rettype = "xml", ...), character(1))
  } else {
    vapply(res$ids, function(z) rentrez::entrez_fetch(db = 'pmc', id = z, rettype = "xml", ...), character(1))
  }
}

bmc_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    url <- sprintf("http://www.microbiomejournal.com/content/download/xml/%s.xml", 
                   strextract(x, "[0-9-]+$"))
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

elife_paper <- function(dois, ...) {
  lapply(dois, function(x) {
    url <- sprintf("http://elife.elifesciences.org/elife-source-xml/%s", x)
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

peerj_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    url <- sprintf("https://peerj.com/articles/%s.xml", strextract(x, "[0-9]+$"))
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

frontiersin_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    url <- sprintf("http://journal.frontiersin.org/article/%s/xml/nlm", x)
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

pensoft_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    httr::content(httr::GET(rcrossref::cr_ft_links(x), ...), as = "text", encoding = "UTF-8")
  })
}

copernicus_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    res <- HEAD(paste0("http://dx.doi.org/", x))
    url <- paste0(res$url, sub("10.5194/", "", x), ".xml")
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

cogent_ft <- function(dois, ...) {
  lapply(dois, function(x) {
    url <- paste0("http://cogentoa.tandfonline.com/doi/xml/", x)
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  })
}

arxiv_ft <- function(dois, basepath, ...) {
  lapply(dois, function(x) {
    url <- sprintf("http://arxiv.org/pdf/%s.pdf", x)
    path <- file.path(basepath, sub("/", "_", sprintf("%s.pdf", x)))
    tmp <- httr::GET(url, write_disk(path, TRUE), ...)
    tmp$request$output$path
  })
}

biorxiv_ft <- function(dois, basepath, ...) {
  lapply(dois, function(x) {
    res <- HEAD(paste0("http://dx.doi.org/", x))
    url <- paste0(res$url, ".full.pdf")
    path <- file.path(basepath, sub("/", "_", sprintf("%s.pdf", x)))
    tmp <- httr::GET(url, write_disk(path, TRUE), ...)
    tmp$request$output$path
  })
}
