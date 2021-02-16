##### utils
plos_abstract <- function(x, ...) {
  z <- rplos::searchplos(q = paste0("id:", x), fl = "abstract", ...)$data
  if ("abstract" %in% names(z)) z$abstract else ""
}
empty_abstracts <- function(fun) function(x) lapply(x, fun)
empty_dois <- empty_abstracts(function(z) list(doi = z, abstract = ""))
empty_ids <- empty_abstracts(function(z) list(id = z, abstract = ""))

# abstract plugins --------------------------------------
plugin_abstract_plos <- function(sources, ids, opts, ...) {
  if (!any(grepl("plos", sources))) return(empty_dois(ids))
  curlopts <- list(...)
  lapply(ids, function(z) {
    opts$x <- z
    opts$callopts <- curlopts
    list(
      doi = z,
      abstract = do.call(plos_abstract, opts)
    )
  })
}

plugin_abstract_scopus <- function(sources, ids, opts, ...) {
  if (!any(grepl("scopus", sources))) return(empty_dois(ids))
  curlopts <- list(...)
  lapply(ids, function(z) {
    opts$x <- z
    opts$curlopts <- curlopts
    list(
      doi = z,
      abstract = do.call(scopus_abstract, opts)
    )
  })
}

plugin_abstract_microsoft <- function(sources, ids, opts, ...) {
  if (!any(grepl("microsoft", sources))) return(empty_ids(ids))
  curlopts <- list(...)
  lapply(ids, function(z) {
    opts$query <- paste0("Id=", z)
    opts <- c(opts, curlopts)
    #opts$sleep <- 1
    list(
      id = z,
      abstract = do.call(microdemic::ma_abstract, opts)
    )
  })
}

plugin_abstract_crossref <- function(sources, ids, opts, ...) {
  if (!any(grepl("crossref", sources))) return(empty_ids(ids))
  lapply(ids, function(z) {
    list(
      id = z,
      abstract = rcrossref::cr_works(dois = z, ...)$data$abstract
    )
  })
}

# opts ignored
plugin_abstract_semanticscholar <- function(sources, ids, opts, ...) {
  if (!any(grepl("semanticscholar", sources))) return(empty_ids(ids))
  assert(ids, "character")
  ss_base <- 'https://api.semanticscholar.org/v1/paper'
  con <- crul::Async$new(urls = file.path(ss_base, ids), opts = list(...))
  out <- con$get()
  jsons <- lapply(out, function(z) jsonlite::fromJSON(z$parse("UTF-8")))
  Map(function(a, b) list(id = a, abstract = b$abstract), ids, jsons)
}

