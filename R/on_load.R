ftxt_cache <- NULL # nocov start
fulltext_store <- NULL

.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("fulltext")
  ftxt_cache <<- x

  # set cache default options
  cache_options_set()

  # create storr fulltext_store
  fulltext_store <<- storr::storr_rds(
    paste0(cache_options_get()$path, "_storr"), 
    mangle_key = TRUE)
} # nocov end
