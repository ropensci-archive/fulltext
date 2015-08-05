#' @title Serialize raw text to other formats, including to disk
#' 
#' @description \code{ft_serialize} helps you convert to various data formats. If 
#' your data is in unparsed XML (i.e., character class), you can convert to 
#' parsed XML. If in XML, you can convert to (ugly-ish) JSON, or a list. In 
#' addition, this function allows you to save to various places, including 
#' Rds files, cached via \code{\link{R.cache}}, or to Redis.
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
#' @param ... Further args passed on to \code{\link[xml2]{read_xml}} or
#' \code{\link[jsonlite]{toJSON}}
#' @return An object of class \code{ft_parsed}
#' @examples \dontrun{
#' dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169',
#' '10.1371/journal.pone.0102976','10.1371/journal.pone.0105225',
#' '10.1371/journal.pone.0102722','10.1371/journal.pone.0033693')
#' res <- ft_get(dois, from='plos')
#'
#' # if articles in xml format, parse the XML
#' (out <- ft_serialize(res, to='xml'))
#' out$plos$data$data[[1]] # the xml
#'
#' # From XML to JSON
#' (out <- ft_serialize(res, to='json'))
#' out$plos$data$data$`10.1371/journal.pone.0087376` # the json
#' jsonlite::fromJSON(out$plos$data$data$`10.1371/journal.pone.0087376`)
#'
#' # To a list
#' out <- ft_serialize(res, to='list')
#' out$plos$data$data[[4]]
#' out$plos$data$data[[4]][[2]]$`article-meta`
#'
#' # To various data stores on disk
#' ## To an .Rds file
#' ft_serialize(res, to='file')
#'
#' ## To local files using R.cache package
#' res_rcache <- ft_serialize(res, to='rcache')
#'
#' ## To Redis
#' res_redis <- ft_serialize(res, to='redis')
#'
#' # Chain together functions
#' doi <- '10.1371/journal.pone.0086169'
#' ft_get(doi, from='plos') %>%
#'    ft_serialize(to='xml') %>%
#'    ft_serialize(to='redis')
#' }

ft_serialize <- function(x, to='xml', from=NULL, ...) {
  is_or(x, c('ft_data', 'ft_parsed'))
  to <- match.arg(to, c('json','xml','list','file','rcache','redis'))
  fmt <- attributes(x$plos$data$data)$format
  tmp <- switch(to,
                xml = to_xml(x, fmt, ...),
                json = to_json(x, fmt, ...),
                list = to_list(x, fmt, ...),
                file = save_file(x, x),
                rcache = save_rcache(x),
                redis = save_redis(x)
  )
  structure(tmp, class = "ft_parsed", type = to, location = attr(tmp, "location"))
}

#' @export
print.ft_parsed <- function(x, ...) {
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data$data))))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(sapply(x, function(y) length(y$data$data)))
  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s", attr(x, "type")), "\n")
  cat(ft_wrap(sprintf("[IDs] \n %s ...", namesprint)), "\n\n")
}

to_xml <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml', 'json'))
  if (fmt == 'xml') {
    lapply(x, function(y) {
      for (i in seq_along(y$data$data)) {
        y$data$data[[i]] <- xml2::read_xml(y$data$data[[i]])
      }
      y$data$data <- unclass(y$data$data)
      return( y )
    })
  } else {
    stop("No conversion from JSON to XML", call. = FALSE)
  }
}

to_json <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml', 'json'))
  if (fmt == 'xml') {
    lapply(x, function(y) {
      for (i in seq_along(y$data$data)) {
        y$data$data[[i]] <- jsonlite::toJSON(xml2::as_list(xml2::read_xml(y$data$data[[i]])), ...)
      }
      return( y )
    })
  } else {
    lapply(x, function(y){
      for (i in seq_along(y$data$data)) {
        y$data$data[[i]] <- jsonlite::toJSON(y$data$data[[i]], ...)
      }
      return( y )
    })
  }
}

to_list <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if (fmt == 'xml') {
    lapply(x, function(y) {
      for (i in seq_along(y$data$data)) {
        y$data$data[[i]] <- xml2::as_list(xml2::read_xml(y$data$data[[i]]))
      }
      y$data$data <- unclass(y$data$data)
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

save_file <- function(x, y, path="~/.fulltext_cache") {
  hash <- digest::digest(x)
  if (!file.exists(path)) dir.create(path, showWarnings = FALSE, recursive = TRUE)
  filepath <- file.path(path, paste0(hash, ".rds"))
  saveRDS(x, filepath)
  structure(x, location = filepath)
}

save_rcache <- function(x) {
  x <- serialize_rcache(x)
  structure(x, location = "~/.Rcache")
}

save_redis <- function(x) {
  tt <- suppressWarnings(tryCatch(redisConnect(), error = function(e) e))
  if (is(tt, "simpleError")) {
    stop("Start redis. Go to your terminal/shell and type redis-server, then hit enter")
  } else {
    x <- serialize_redis(x)
    redisClose()
    structure(x, location = "Redis")
  }
}

serialize_rcache <- function(x) {
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$data)) {
      x[[i]]$data$data <- NULL
    } else {
      for (j in seq_along(x[[i]]$data$data)) {
        nn <- paste(names(x[i]), names(x[[i]]$data$data[j]), sep = "_")
        x[[i]]$data$data[[j]] <- saveCache(object = x[[i]]$data$data[[j]], key = list(nn))
      }
    }
  }
  return( x )
}

serialize_redis <- function(x) {
  for (i in seq_along(x)) {
    if (is.null( x[[i]]$data )) {
      x[[i]]$data$data <- NULL
    } else {
      for (j in seq_along(x[[i]]$data$data)) {
        nn <- paste(names(x[i]), names(x[[i]]$data$data[j]), sep = "_")
        x[[i]]$data$data[[j]] <- redis_set_(nn, x[[i]]$data$data[[j]])
      }
    }
  }
  return( x )
}

redis_set_ <- function(x, y) {
  redisSet(x, y)
  x
}

#' @export
#' @rdname ft_serialize
ft_get_keys <- function(x) {
  lapply(x, function(z) names(z$data))
}
