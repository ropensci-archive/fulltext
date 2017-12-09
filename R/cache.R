#' Cache blobs of json, xml or pdfs of text from `ft_get()` function
#'
#' @name cache
#'
#' @param cache (logical) If `TRUE`, cache results, if not objects saved 
#' within R session.
#' @param backend (character) One of rds, rcache, redis
#' @param path (character) End of directory path. Default: "fulltext". 
#' See Details.
#' @param cachetype The cache type
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
#' ft_get('10.1371/journal.pone.0086169', from='plos', cache=FALSE)
#' ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE)
#'
#' cache_options_set(backend="redis")
#' cache_options_get()
#' (x <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="redis"))
#' x %>% collect()
#'
#' cache_options_set(backend="rcache")
#' cache_options_get()
#' (x <- ft_get('10.1371/journal.pone.0086169', from='plos'))
#' x %>% collect()
#'
#' # Many different sources
#' (res <- ft_search(query='ecology', from='entrez'))
#' cache_options_set(backend="rds")
#' out <- ft_get(res)
#' out$entrez
#' out %>% collect() %>% chunks("title")
#' 
#' 
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
cache_options_set <- function(cache = TRUE, backend = "rds", path = "fulltext") {
  options(ft_cache = cache)
  options(ft_backend = if (!cache) NULL else backend)
  if (!cache || backend %in% c("redis", "rcache")) {
    path <- NULL 
  } else {
    ftxt_cache$cache_path_set(path)
    path <- ftxt_cache$cache_path_get()
  }
  options(ft_path = path)
}

#' @export
#' @rdname cache
cache_options_get <- function(){
  list(cache = getOption("ft_cache"),
       backend = getOption("ft_backend"),
       path = getOption("ft_path")
  )
}

############# save cache
cache_save <- function(obj, backend, path, db = NULL) {
  backend <- match.arg(backend, choices = c('rds', 'rcache', 'redis'))
  switch(backend,
         rds = save_rds(x = obj, path),
         rcache = save_rcache2(obj),
         redis = save_redis2(obj)
         # sqlite = save_sqlite(db=db, obj, key)
  )
}

save_rds <- function(x, path) {
  # make base caching directory if it doesn't exist
  ftxt_cache$mkdir()

  # make hash of object
  hash <- digest::digest(x)

  # make file path
  filepath <- file.path(path, paste0(hash, ".rds"))

  # save as compressed binary 
  saveRDS(object = x, file = filepath)

  # return file path
  return( filepath )
}

save_redis2 <- function(x) {
  tt <- suppressWarnings(tryCatch(redisConnect(), error = function(e) e))
  if (inherits(tt, "simpleError")) {
    stop("Start redis. Go to your terminal/shell and type redis-server, then hit enter")
  } else {
    key <- digest::digest(x)
    redisSet(key, x)
    redisClose()
    return( key )
  }
}

save_rcache2 <- function(x){
  key <- digest::digest(x)
  saveCache(object = x, key = list(key))
}

# save_sqlite <- function(db, x, y) dbInsert(db, key=y, value=x)

############# get cache
cache_get <- function(key=NULL, backend=NULL, path=NULL, db=NULL) {
  if (is.null(key)) {
    NULL
  } else {
    backend <- match.arg(backend, choices = c('rds', 'rcache', 'redis'))
    key <- path.expand(key)
    switch(backend,
           rds = get_rds(key),
           rcache = get_rcache(key),
           redis = get_redis(key)
           # sqlite = get_sqlite(key, db=db)
    )
  }
}

get_rds <- function(z) if (is.null(z)) return(NULL) else readRDS(z)

get_redis <- function(key) {
  if (is.null(key)) {
    NULL
  } else {
    redisConnect()
    tt <- suppressWarnings(tryCatch(redisConnect(), error = function(e) e))
    if (is(tt, "simpleError")) {
      stop("You need to start redis. Go to your terminal/shell and type redis-server, then hit enter")
    } else {
      nn <- redisGet(key)
      redisClose()
      if (!is.null(nn)) {
        nn
      } else {
        NULL
      }
    }
  }
}

get_rcache <- function(key) loadCache(list(key))

# get_sqlite <- function(cache, key, db)
# {
#   if(cache){
#     if(dbExists(db, key)){
#       tmp <- tryCatch(dbFetch(db, key), error = function(e) e)
#       if(grepl('subscript out of bounds', as.character(tmp))){ NULL } else { tmp }
#     }
#   } else
#   { NULL }
# }

############# cache clear
#' @export
#' @rdname cache
cache_clear <- function(cachetype=NULL){
  message("not operational yet")
#   if(is.null(cachetype))
#     cachetype <- getOption('cachetype')
#   if(is.null(cachetype))
#     stop("Sorry, can't find your cache type. Either enter
#   		a type or keep a type in your .Rprofile file")
#
#   switch(cachetype,
#          local = X, # i.e., digest
#          r.cache = X,
#          redis = X,
#          sqlite = X)
}
