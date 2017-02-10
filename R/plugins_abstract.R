# abstract plugins --------------------------------------
plugin_abstract_scopus <- function(dois, opts, ...) {
  lapply(dois, function(z) {
    opts$x <- z
    list(
      doi = z, 
      abstract = do.call(scopus_abstract, opts)
    )
  })
}
