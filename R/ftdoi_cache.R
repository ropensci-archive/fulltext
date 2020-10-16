#' @title Caching
#'
#' @description Manage cached `ftdoi` files with \pkg{hoardr}
#'
#' @export
#' @name ftdoi_cache
#' @family ftdoi
#' @details The dafault cache directory is
#' `paste0(rappdirs::user_cache_dir(), "/R/ftdoi")`, but you can set
#' your own path using `cache_path_set()`
#'
#' `cache_delete` only accepts 1 file name, while
#' `cache_delete_all` doesn't accept any names, but deletes all files.
#' For deleting many specific files, use `cache_delete` in a [lapply()]
#' type call
#'
#' @section Useful user functions:
#'
#' - `ftdoi_cache$cache_path_get()` get cache path
#' - `ftdoi_cache$cache_path_set()` set cache path. You can set the 
#'    entire path directly via the `full_path` arg like 
#'   `ftdoi_cache$cache_path_set(full_path = "your/path")`
#' - `ftdoi_cache$list()` returns a character vector of full
#'    path file names
#' - `ftdoi_cache$files()` returns file objects with metadata
#' - `ftdoi_cache$details()` returns files with details
#' - `ftdoi_cache$delete()` delete specific files
#' - `ftdoi_cache$delete_all()` delete all files, returns nothing
#'
#' @examples \dontrun{
#' ftdoi_cache
#'
#' # list files in cache
#' ftdoi_cache$list()
#'
#' # delete certain database files
#' # ftdoi_cache$delete("file path")
#' # ftdoi_cache$list()
#'
#' # delete all files in cache
#' # ftdoi_cache$delete_all()
#' # ftdoi_cache$list()
#' }
NULL
