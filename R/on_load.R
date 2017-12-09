ftxt_cache <- NULL # nocov start
cache_options_set(FALSE)
.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("fulltext")
  ftxt_cache <<- x
} # nocov end
