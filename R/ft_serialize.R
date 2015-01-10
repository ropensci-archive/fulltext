#' Serialize raw text to other formats, including to disk.
#'
#' @importFrom digest digest
#' @importFrom rredis redisClose redisSet redisConnect redisGet
#' @importFrom R.cache saveCache loadCache
#' @export
#'
#' @param x Input object, output from a call to \code{ft_get}. Required.
#' @param to (character) Format to serialize to. One of list, xml, json, ... Required.
#' Output to xml returns object of class XMLInternalDocument.
#' @param from (character) Format \code{x} is currently in. Function attempts to use metadata
#' provided, or guess from data itself. Optional. CURRENTLY IGNORED.
#' @param ... Further args passed on to \code{XML::xmlParse} or \code{jsonlite::toJSON}
#' @examples \donttest{
#' dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169',
#' '10.1371/journal.pone.0102976','10.1371/journal.pone.0105225',
#' '10.1371/journal.pone.0102722','10.1371/journal.pone.0033693')
#' res <- ft_get(ids=dois, from='plos')
#'
#' # From XML to JSON
#' (out <- ft_serialize(res, to='json'))
#' out$plos$data$`10.1371/journal.pone.0087376` # the json
#' jsonlite::fromJSON(out$plos$data$`10.1371/journal.pone.0087376`)
#'
#' # Parse raw xml to XMLInternalDocument class
#' out <- ft_serialize(res, to='xml')
#' out$plos$data[[1]]
#'
#' # To a list
#' out <- ft_serialize(res, to='list')
#' out$plos$data[[1]]
#'
#' # To various data stores on disk
#' ## To .Rds files
#' ft_serialize(res, to='file')
#'
#' ## To local files using R.cache package
#' res_rcache <- ft_serialize(res, to='rcache')
#'
#' ## To Redis
#' res_redis <- ft_serialize(res, to='redis')
#'
#' # Sizes
#' pryr::object_size(res)
#' pryr::object_size(res_rcache)
#' pryr::object_size(res_redis)
#'
#' # Chain together functions
#' doi <- '10.1371/journal.pone.0086169'
#' ft_get(doi, from='plos') %>%
#'    ft_serialize(to='xml') %>%
#'    ft_serialize(to='redis')
#' }

ft_serialize <- function(x, to='xml', from=NULL, ...)
{
  match.arg(to, c('json','xml','list','file','rcache','redis'))
  fmt <- attributes(x$plos$data)$format
  tmp <- switch(to,
                xml = to_xml(x, fmt, ...),
                json = to_json(x, fmt, ...),
                list = to_list(x, fmt, ...),
                file = save_file(x, x),
                rcache = save_rcache(x),
                redis = save_redis(x)
  )
  structure(tmp, class="ft_parsed", type=to, location=attr(tmp, "location"))
}

#' @export
print.ft_parsed <- function(x, ...) {
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data))))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(sapply(x, function(y) length(y$data)))
  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s", attr(x, "type")), "\n")
  cat(ft_wrap(sprintf("[IDs] \n %s ...", namesprint)), "\n\n")
}

to_xml <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y){
      y$data <- lapply(y$data, XML::xmlParse)
      return( y )
    })
  } else {
    stop("No conversion from JSON to XML", call. = FALSE)
  }
}

to_json <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y){
      y$data <- lapply(y$data, function(z){
        ztmp <- XML::xmlToList(z)
        jsonlite::toJSON(ztmp, ...)
      })
      return( y )
    })
  } else {
    lapply(x, function(y){
      y$data <- lapply(y$data, jsonlite::toJSON, ...)
      return( y )
    })
  }
}

to_list <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y){
      y$data <- lapply(y$data, XML::xmlToList, ...)
      return( y )
    })
  } else {
    lapply(x, function(y){
      y$data <- lapply(y$data, function(z){
        jsonlite::fromJSON(z, FALSE, ...)
      })
      return( y )
    })
  }
}

save_file <- function(x, y, path="~/.fulltext_cache")
{
  hash <- digest::digest(x)
  if(!file.exists(path)) dir.create(path, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(path, paste0(hash, ".rds"))
  saveRDS(x, filepath)
  structure(x, location=filepath)
}

save_rcache <- function(x){
  x <- serialize_rcache(x)
  structure(x, location="~/.Rcache")
}

save_redis <- function(x){
  tt <- suppressWarnings(tryCatch(redisConnect(), error=function(e) e))
  if(is(tt, "simpleError")){
    stop("Start redis. Go to your terminal/shell and type redis-server, then hit enter")
  } else
  {
    x <- serialize_redis(x)
    redisClose()
    structure(x, location="Redis")
  }
}

serialize_rcache <- function(x){
  for(i in seq_along(x)){
    if(is.null( x[[i]]$data )){
      x[[i]]$data <- "none"
    } else {
      for(j in seq_along(x[[i]]$data)){
        nn <- paste(names(x[i]), names(x[[i]]$data[j]), sep = "_")
        x[[i]]$data[[j]] <- saveCache(object = x[[i]]$data[[j]], key = list(nn))
      }
    }
  }
  return( x )
}

serialize_redis <- function(x){
  for(i in seq_along(x)){
    if(is.null( x[[i]]$data )){
      x[[i]]$data <- "none"
    } else {
      for(j in seq_along(x[[i]]$data)){
        nn <- paste(names(x[i]), names(x[[i]]$data[j]), sep = "_")
        x[[i]]$data[[j]] <- redis_set_(nn, x[[i]]$data[[j]])
      }
    }
  }
  return( x )
}

redis_set_ <- function(x, y){
  redisSet(x, y)
  x
}

#' @export
#' @rdname ft_serialize
ft_get_keys <- function(x){
  lapply(x, function(z) names(z$data))
}
