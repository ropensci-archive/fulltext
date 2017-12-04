# abstract plugins --------------------------------------
plugin_abstract_plos <- function(sources, ids, opts, ...) {
  if (any(grepl("plos", sources))) {
    lapply(ids, function(z) {
      opts$x <- z
      list(
        doi = z, 
        abstract = do.call(plos_abstract, opts)
      )
    })
  } else {
    lapply(ids, function(z) {
      list(doi = z, abstract = "")
    })
  }
}

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
      #opts$sleep <- 1
      list(
        id = z, 
        abstract = do.call(microdemic::ma_abstract, opts)
      )
    })
  } else {
    lapply(ids, function(z) {
      list(id = z, abstract = "")
    })
  }
}

plugin_abstract_crossref <- function(sources, ids, opts, ...) {
  if (any(grepl("crossref", sources))) {
    lapply(ids, function(z) {
      list(
        id = z, 
        abstract = rcrossref::cr_works(dois = z)$data$abstract
      )
    })
  } else {
    lapply(ids, function(z) {
      list(id = z, abstract = "")
    })
  }
}

##### utils
plos_abstract <- function(x, ...) {
  rplos::searchplos(q = paste0("doi:", x), fl = "abstract", ...)$data[[1]]
}
