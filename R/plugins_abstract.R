# abstract plugins --------------------------------------
plugin_abstract_scopus <- function(sources, ids, opts, ...) {
  if (any(grepl("scopus", sources))) {
    lapply(ids, function(z) {
      opts$x <- z
      list(
        doi = z, 
        abstract = do.call(scopus_abstract, opts)
      )
    })
  } else {
    lapply(ids, function(z) {
      list(doi = z, abstract = "")
    })
  }
}

plugin_abstract_microsoft <- function(sources, ids, opts, ...) {
  if (any(grepl("microsoft", sources))) {
    lapply(ids, function(z) {
      opts$query <- paste0("Id=", z)
      list(
        id = z, 
        abstract = do.call(microsoft_abstract, opts)
      )
    })
  } else {
    lapply(ids, function(z) {
      list(id = z, abstract = "")
    })
  }
}
