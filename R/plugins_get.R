plugin_get_crossref <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("entrez", sources))){
    opts$ids <- ids
    out <- do.call(entrez_get, opts)
    attr(out, "format") <- "xml"
    list(found = length(out), dois = NULL, data = out, opts = opts)
  } else {
    list(found = NULL, dois = NULL, data = NULL, opts = opts)
  }
}

plugin_get_plos <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("plos", sources))){
    check_dois(ids)
    opts$doi <- ids
    opts$callopts <- callopts
    out <- do.call(plos_fulltext, opts)
    attr(out, "format") <- "xml"
    list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
  } else {
    list(found = NULL, dois = NULL, data = NULL, opts = opts)
  }
}

construct_paths <- function(co, x){
  if(!co$cache){
    list(backend = NULL, 
         path = "session",
         data = x)
  } else {
    list(backend = co$backend,
         path = cache_save(obj = x, backend = co$backend, path = co$path),
         data = NULL)
  }
}

plugin_get_entrez <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("entrez", sources))){
    opts$ids <- ids
    out <- as.list(do.call(entrez_get, opts))
    attr(out, "format") <- "xml"
    list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
  } else {
    list(found = NULL, dois = NULL,  data = NULL, opts = opts)
  }
}

entrez_get <- function(ids){
  res <- entrez_search(db="pmc", term=paste0(sprintf('%s[doi]', ids), collapse = "|"))
  vapply(res$ids, function(z) entrez_fetch(db = 'pmc', id=z, rettype = "xml"), character(1))
}

plugin_get_bmc <- function(sources, query, opts, ...){
  callopts <- list(...)
  if(any(grepl("bmc", sources))){
    opts$uris <- query
    opts$raw <- TRUE
    out <- do.call(bmc_xml, opts)
    attr(out, "format") <- "xml"
    dois <- sapply(out, function(x) {
      xpathSApply(xmlParse(x), "//fm//bibl//pubid[@idtype='doi']", xmlValue)
    })
    list(found = length(out), dois = dois, data = out, opts = opts)
  } else {
    list(found = NULL, dois = NULL,  data = NULL, opts = opts)
  }
}

plugin_get_elife <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("elife", sources))){
    opts$doi <- ids
    out2 <- lapply(ids, elife_paper)
    names(out2) <- ids
    attr(out2, "format") <- "xml"
    list(found = length(out2), dois = NULL, data = out2, opts = opts)
  } else {
    list(found = NULL, dois = NULL, data = NULL, opts = opts)
  }
}

elife_paper <- function(doi) {
  url <- sprintf("http://elife.elifesciences.org/elife-source-xml/%s", doi)
  httr::content(GET(url), as="text")
}
