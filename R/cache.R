#' Cache blobs of json, xml or pdfs of text from `ft_get()` function
#'
#' @name cache
#'
#' @param path (character) End of directory path. Default: "fulltext". 
#' See Details.
#' @param backend (character) Only "ext" supported for now.
#' @param overwrite (logical) overwrite cached file or not. Default: `FALSE`
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
#' @section Useful user functions for managing cached files:
#' \itemize{
#'  \item `ftxt_cache$list()` returns a character vector of full
#'  path file names
#'  \item `ftxt_cache$files()` returns file objects with metadata
#'  \item `ftxt_cache$details()` returns files with details
#'  \item `ftxt_cache$delete()` delete specific files
#'  \item `ftxt_cache$delete_all()` delete all files, returns nothing
#' }
#'
#'
#' @examples \dontrun{
#' # Manage cached files with this object
#' ftxt_cache
#'
#' # list files in cache
#' ftxt_cache$list()
#' 
#' # list details of files in cache
#' ftxt_cache$details()
#'
#' # delete certain database files
#' # ftxt_cache$delete("file path")
#' # ftxt_cache$list()
#'
#' # delete all files in cache
#' # ftxt_cache$delete_all()
#' # ftxt_cache$list()
#' }

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
           # rds = get_rds(key)
           # rcache = get_rcache(key),
           # redis = get_redis(key)
           # sqlite = get_sqlite(key, db=db)
    # )
  }
}

get_ext <- function(x) {
  switch(
    strextract(x, "\\..+"),
    '.xml' = xml2::read_xml(x),
    '.pdf' = pdftools::pdf_text(x),
    '.txt' = paste0(readLines(x), collapse = " ")
  )
}
