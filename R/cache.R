#' Cache blobs of json, xml or pdfs of text from ft_get() function
#' 
#' @name cache
#' 
#' @param cache (logical) If TRUE, cache results, if not objects saved within R session.
#' @param backend (character) One of local, rcache, redis
#' @param path path to local storage. used only if \code{backend="rds"}
#' @param cachetype The cache type
#'
#' @examples \donttest{
#' ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=FALSE)
#' ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=TRUE)
#' 
#' cache_options_set(backend="redis")
#' (x <- ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="redis"))
#' x %>% collect()
#' }

#' @export
#' @rdname cache
cache_options_set <- function(cache = TRUE, backend = "rds", path="~/.fulltext"){
  options(ft_cache = cache)
  options(ft_backend = if(!cache) NULL else backend)
  options(ft_path = if(!cache || "redis" %in% backend) NULL else path)
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
cache_save <- function(cache, key, obj, backend, path, db)
{
  if(cache){
    backend <- match.arg(backend, choices=c('rds', 'rcache', 'redis'))
    switch(backend,
           rds = save_rds(obj, path),
           rcache = save_rcache2(obj, key),
           redis = save_redis2(obj)
#            ,
#            sqlite = save_sqlite(db=db, obj, key)
    )
  } else { NULL }
}

save_rds <- function(x, path="~/")
{
  hash <- digest::digest(x)
  filepath <- file.path(path, paste0(hash, ".rds"))
  saveRDS(object=x, file=filepath)
  return( filepath )
}

save_rcache2 <- function(x, y){
  saveCache(object=x)
}

save_redis2 <- function(x){
  tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
  if(is(tt, "simpleError")){
    stop("Start redis. Go to your terminal/shell and type redis-server, then hit enter")
  } else
  {
    key <- digest::digest(x)
    redisSet(key, x)
    redisClose()
    return( key )
  }
}

# save_sqlite <- function(db, x, y) dbInsert(db, key=y, value=x)

############# get cache
cache_get <- function(key=NULL, backend=NULL, path=NULL, db=NULL)
{
  backend <- match.arg(backend, choices=c('rds', 'rcache', 'redis'))
  switch(backend,
         rds = get_rds(path),
         rcache = get_rcache(key),
         redis = get_redis(key)
#          ,
#          sqlite = get_sqlite(key, db=db)
  )
}

get_rds <- function(z){
  if(is.null(z))
    NULL
  else 
    readRDS(z)
}

get_redis <- function(key)
{
  if(is.null(key)){
    NULL
  } else {
    redisConnect()
    tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
    if(is(tt, "simpleError")){
      stop("You need to start redis. Go to your terminal/shell and type redis-server, then hit enter")
    } else {
      nn <- redisGet(key)
      redisClose()
      if(!is.null(nn)){ 
        nn 
      } else { 
        NULL 
      }
    }
  }
}

get_rcache <- function(cache, key){
  if(cache){
    loadCache(list(key))
  } else { NULL }
}

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
