#' Inspect and manage cached files
#'
#' @export
#' @name ftxt_cache
#' 
#' @seealso [cache], [cache_file_info()]
#' @family caching-functions
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
#' @examples \dontrun{
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
NULL
