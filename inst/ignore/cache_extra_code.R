save_as_ext <- function(z, path, type) {
  # make base caching directory if it doesn't exist
  ftxt_cache$mkdir()

  filepaths <- vector("list", length = length(z))
  for (i in seq_along(z)) {
    if (inherits(z[[i]], "exists_obj")) {
      filepaths[[i]] <- z[[1]]
    } else {
      # make hash of the id
      hash <- digest::digest(c(names(z)[i], type))

      # make file path
      file_ext <- get_file_ext(type)
      filepaths[[i]] <- file.path(path, paste0(hash, file_ext))

      # save file
      saveRDS(object = z[[i]], file = filepaths[[i]]) 
    }
  }

  # return file path
  return( stats::setNames(filepaths, names(z)) )
}

cache_save <- function(obj, backend, path, type = NULL, db = NULL) {
  backend <- match.arg(backend, choices = c('ext', 'rds', 'rcache', 'redis'))
  switch(
    backend,
    ext = save_as_ext(obj, path, type),
    rds = save_rds(obj, path, type),
    rcache = save_rcache2(obj),
    redis = save_redis2(obj)
  )
}

save_rds <- function(z, path, type) {
  # make base caching directory if it doesn't exist
  ftxt_cache$mkdir()

  filepaths <- vector("list", length = length(z))
  for (i in seq_along(z)) {
    if (inherits(z[[i]], "exists_obj")) {
      filepaths[[i]] <- z[[1]]
    } else {
      # make hash of the id
      hash <- digest::digest(c(names(z)[i], type))

      # make file path
      filepaths[[i]] <- file.path(path, paste0(hash, ".rds"))

      # save as compressed binary 
      saveRDS(object = z[[i]], file = filepaths[[i]]) 
    }
  }

  # return file path
  return( stats::setNames(filepaths, names(z)) )
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
#           a type or keep a type in your .Rprofile file")
#
#   switch(cachetype,
#          local = X, # i.e., digest
#          r.cache = X,
#          redis = X,
#          sqlite = X)
}

to_text <- function(x) httr::content(x, as = "text", encoding = "UTF-8")
to_raw <- function(x) httr::content(x, as = "raw")
