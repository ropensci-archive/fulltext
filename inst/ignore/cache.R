#' Cache blobs of json, xml or pdfs of text from ft_get() function
#' 
#' @export
#' 
#' @param cache (logical) If TRUE, cache results, if not objects saved within R session.
#' @param backend (character) One of 
#' @param ... Further args passed on to each backend
#'
#' @examples \donttest{
#' ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=FALSE)
#' ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=TRUE)
#' }

cache_text <- function(cache, backend = "rcache", ...)
{
  ""
}


############# save cache
#' Save results to chosen backend
#' 
#' @param cache Logical
#' @param key Key from url + args 
#' @param obj Object to save
#' @param backend One of local, redis, couchdb.
#' @param path Path for local storage. Only used when backend='local'
#' @param db Database name for CouchDB or SQLlite
cache_save <- function(cache, key, obj, backend, path, db)
{
  if(cache){
    backend <- match.arg(backend, choices=c('local', 'rcache', 'redis', 'sqlite', 'couchdb'))
    switch(backend,
           local = save_local(obj, key, path),
           rcache = save_rcache(obj, key),
           redis = save_redis(key, obj),
           sqlite = save_sqlite(db=db, obj, key)
    )
  } else { NULL }
}

#' Save results locally
#' @import digest
#' @param x Output from API call
#' @param y Cache key
#' @keywords internal
save_local <- function(x, y, path="~/")
{
  hash <- digest::digest(y)
  filepath <- paste(path, hash, ".rds", sep="")
  saveRDS(object=x, file=filepath)
}

#' Save locally using R.cache
#' @import R.cache
#' @param x Output from API call
#' @param y Cache key
#' @keywords internal
save_rcache <- function(x, y){
  saveCache(object=x)
}

#' Save results to Redis backend
#' @import rredis
#' @param x key
#' @param y object
#' @keywords internal
save_redis <- function(x, y){
  tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
  if(is(tt, "simpleError")){
    stop("Start redis. Go to your terminal/shell and type redis-server, then hit enter")
  } else
  {
    redisSet(x, y)
    redisClose()
  }
}

#' Save results to chosen backend
#' @import filehashSQLite
#' @param db a database name
#' @param x Object to save
#' @param y Key to save on
#' @keywords internal
save_sqlite <- function(db, x, y) dbInsert(db, key=y, value=x)

############# get cache
#' Search for data in a chosen backend
#' 
#' @param cache Logical
#' @param key Key from url + args 
#' @param backend One of local, redis, couchdb, mongodb, sqlite.
#' @param path An optional path to store data in. Only applicable for X and Y.
#' @param db Database name for CouchDB or SQLlite
#' @export
cache_get <- function(cache, key, backend, path, db)
{
  backend <- match.arg(backend, choices=c('local', 'rcache', 'redis', 'sqlite', 'couchdb'))
  switch(backend,
         local = get_local(cache, key, path),
         rcache = get_rcache(cache, key),
         redis = get_redis(cache, key),
         sqlite = get_sqlite(cache, key, db=db),
         couchdb = get_couch(cache, key, db=db)
  )
}

#' Get value from local storage based on key
#' @import digest
#' @param cache Logical
#' @param key Key from url + args
#' @examples \dontrun{
#' key = "http://api.plos.org/search?q=author:Ethan%20White&rows=1&wt=json"
#' path = "~/scottscache"
#' golocal2(TRUE, key, path)
#' }
#' @keywords internal
get_local <- function(cache, key, path="~/")
{
  if(cache){
    hash <- digest::digest(key)
    stored_hashes <- list.files(path, full.names=TRUE, pattern=".rds")
    getname <- function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])]
    stored_hashes_match <- gsub("\\.rds", "", sapply(stored_hashes, getname, USE.NAMES=FALSE))
    if(length(stored_hashes) == 0){
      NULL 
    } else
    {  
      tt <- stored_hashes[stored_hashes_match %in% hash]
      if(identical(tt, character(0))){ NULL } else {
        tmp <- readRDS(tt)
        return( tmp )
      }
    }
  } else { NULL }
}

#' Get local results using R.cache
#' @import R.cache
#' @param cache Logical
#' @param key Key from url + args
#' @keywords internal
get_rcache <- function(cache, key){
  if(cache){
    loadCache(list(key))
  } else { NULL }
}

#' Get redis cached data
#' @import rredis
#' @param cache Logical
#' @param key Key from url + args 
#' @keywords internal
get_redis <- function(cache, key)
{
  if(cache){
    tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
    if(is(tt, "simpleError")){
      stop("You need to start redis. Go to your terminal/shell and type redis-server, then hit enter")
    } else
    {
      nn <- redisGet(key)
      redisClose()
      if(!is.null(nn)){ nn } else
      { NULL }
    }
  } else
  { NULL }
}

#' Get value from SQlite storage based on key
#' @import filehashSQLite
#' @param cache Logical
#' @param key Key from url + args
#' @param db Database name
#' @keywords internal
get_sqlite <- function(cache, key, db)
{
  if(cache){
    if(dbExists(db, key)){
      tmp <- tryCatch(dbFetch(db, key), error = function(e) e)
      if(grepl('subscript out of bounds', as.character(tmp))){ NULL } else { tmp }
    }
  } else
  { NULL }
}

############# cache clear
#' @export
#' @rdname cache_text
cache_clear <- function(cachetype=NULL){
  if(is.null(cachetype))
    cachetype <- getOption('cachetype')
  if(is.null(cachetype))
    stop("Sorry, can't find your cache type. Either enter 
  		a type or keep a type in your .Rprofile file")
  
  switch(cachetype, 
         local = X, # i.e., digest
         r.cache = X,
         redis = X,
         sqlite = X)
}
