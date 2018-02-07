#' Set or get cache options
#'
#' @name cache
#'
#' @param path (character) End of directory path. Default: "fulltext". 
#' See Details.
#' @param backend (character) Only "ext" supported for now.
#' @param overwrite (logical) overwrite cached file or not. Default: `FALSE`
#' 
#' @seealso [ftxt_cache]
#' 
#' @section Managing cached files:
#' The dafault cache directory is `paste0(rappdirs::user_cache_dir(), "/R/fulltext")`, 
#' but you can set your own path using `cache_path_set()`
#'
#' `cache_delete` only accepts 1 file name, while
#' `cache_delete_all` doesn't accept any names, but deletes all files.
#' For deleting many specific files, use `cache_delete` in a [lapply()]
#' type call
#'
#'
#' @examples \dontrun{
#' cache_options_get()
#' cache_options_set(path = "foobar")
#' cache_options_get()
#' }
NULL

#' @export
#' @rdname cache
cache_options_set <- function(path = "fulltext", backend = "ext", overwrite = FALSE) {
  options(ft_cache = TRUE)
  options(ft_backend = "ext")
  ftxt_cache$cache_path_set(path)
  path <- ftxt_cache$cache_path_get()
  options(ft_path = path)
  options(ft_overwrite = overwrite)
}

#' @export
#' @rdname cache
cache_options_get <- function(){
  list(
    cache = getOption("ft_cache"),
    backend = getOption("ft_backend"),
    path = getOption("ft_path"),
    overwrite = getOption("ft_overwrite")
  )
}

############# save cache
get_file_ext <- function(x) {
  switch(x, xml = ".xml", pdf = '.pdf', plain = ".txt")
}

cache_get <- function(key=NULL, backend=NULL, path=NULL, db=NULL) {
  if (is.null(key)) {
    NULL
  } else {
    backend <- match.arg(backend, choices = 'ext')
    key <- path.expand(key)
    switch(backend, ext = get_ext(key))
  }
}

get_ext <- function(x) {
  switch(
    strextract(basename(x), "\\..+$"),
    '.xml' = xml2::read_xml(x),
    '.pdf' = pdftools::pdf_text(x),
    '.txt' = paste0(readLines(x), collapse = " ")
  )
}

# check that directory is created for the cache in case user changes location
check_cache <- function() ftxt_cache$mkdir()
