# #' Get value from local storage based on key
# #' @import digest
# #' @param cache Logical
# #' @param key Key from url + args
# #' @examples \dontrun{
# #' key = "http://api.plos.org/search?q=author:Ethan%20White&rows=1&wt=json"
# #' path = "~/scottscache"
# #' golocal2(TRUE, key, path)
# #' }
# #' @keywords internal
# get_rds <- function(cache, key, path="~/")
# {
#   if(cache){
#     hash <- digest::digest(key)
#     stored_hashes <- list.files(path, full.names=TRUE, pattern=".rds")
#     getname <- function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])]
#     stored_hashes_match <- gsub("\\.rds", "", sapply(stored_hashes, getname, USE.NAMES=FALSE))
#     if(length(stored_hashes) == 0){
#       NULL 
#     } else
#     {  
#       tt <- stored_hashes[stored_hashes_match %in% hash]
#       if(identical(tt, character(0))){ NULL } else {
#         tmp <- readRDS(tt)
#         return( tmp )
#       }
#     }
#   } else { NULL }
# }
# 
# #' Get local results using R.cache
# #' @import R.cache
# #' @param cache Logical
# #' @param key Key from url + args
# #' @keywords internal
# get_rcache <- function(cache, key){
#   if(cache){
#     loadCache(list(key))
#   } else { NULL }
# }
# 
# #' Get redis cached data
# #' @import rredis
# #' @param cache Logical
# #' @param key Key from url + args 
# #' @keywords internal
# get_redis <- function(cache, key)
# {
#   if(cache){
#     tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
#     if(is(tt, "simpleError")){
#       stop("You need to start redis. Go to your terminal/shell and type redis-server, then hit enter")
#     } else
#     {
#       nn <- redisGet(key)
#       redisClose()
#       if(!is.null(nn)){ nn } else
#       { NULL }
#     }
#   } else
#   { NULL }
# }
# 
# #' Get value from SQlite storage based on key
# #' @import filehashSQLite
# #' @param cache Logical
# #' @param key Key from url + args
# #' @param db Database name
# #' @keywords internal
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

