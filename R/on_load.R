ftxt_cache <- NULL # nocov start
ftdoi_cache <- NULL
fulltext_store <- ftdoi_doi_prefixes <- ftdoi_urls <- ftdoi_doi_issn <- NULL

.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("fulltext")
  ftxt_cache <<- x

  z <- hoardr::hoard()
  z$cache_path_set("ftdoi")
  ftdoi_cache <<- z

  # set cache default options
  cache_options_set()

  # create storr fulltext_store
  fulltext_store <<- storr::storr_rds(
    paste0(cache_options_get()$path, "_storr"), 
    mangle_key = TRUE)
  # ftdoi storr's
  ftdoi_doi_prefixes <<- storr::storr_rds(
    file.path(z$cache_path_get(), "_doi_prefixes"),
    mangle_key = TRUE)
  ftdoi_urls <<- storr::storr_rds(
    file.path(z$cache_path_get(), "_urls"),
    mangle_key = TRUE)
  ftdoi_doi_issn <<- storr::storr_rds(
    file.path(z$cache_path_get(), "_doi_issn"),
    mangle_key = TRUE)
} # nocov end
