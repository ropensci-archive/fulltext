#' Serialize raw text to other formats, including to disk.
#' 
#' @export
#' 
#' @param x Input object, output from a call to \code{ft_get}. Required.
#' @param to (character) Format to serialize to. One of list, xml, json, data.frame, ... Required. 
#' Output to xml returns object of class XMLInternalDocument.
#' @param from (character) Format \code{x} is currently in. Function attempts to use metadata
#' provided, or guess from data itself. Optional. CURRENTLY IGNORED.
#' @param ... Further args passed on to \code{XML::xmlParse} or \code{jsonlite::toJSON}
#' @examples \donttest{
#' dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169')
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
#' ## To Redis
#' ft_serialize(res, to='redis')
#' 
#' ## To SQLite
#' ft_serialize(res, to='sqlite')
#' }

ft_serialize <- function(x, to='xml', from=NULL, ...)
{
  fmt <- attributes(x$plos$data)$format
  tmp <- switch(to, 
                xml = to_xml(x, fmt, ...),
                json = to_json(x, fmt, ...),
                list = to_list(x, fmt, ...),
                file = save_file(x, x)
  )
  structure(tmp, class="ft_parsed", type=to)
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
  attr(x, "location") <- filepath
  return( x )
}

#' Print brief summary of ft_parsed object
#'
#' @param x Input...
#' @param ... Ignored.
#' @method print ft_parsed
#' @export

print.ft_parsed <- function(x, ...) {
  locpath <- if(attr(x, "type") == "file") attr(x, "location") else "R session"
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data))))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(sapply(x, function(y) length(y$data)))
#   lengths <- unlist( sapply(x, function(y){ if(!is.null(y$data)) vapply(y$data, nchar, 1) else NULL }) )
#   cat(sprintf("Min. Length: %s - Max. Length: %s", min(lengths), max(lengths)), "\n")
  cat(sprintf("[Documents] %s", totgot), "\n")
  cat(sprintf("[Location] %s", locpath), "\n")
  cat(ft_wrap(sprintf("IDs:\n %s ...", namesprint)), "\n\n")
  cat("NOTE: extract xml strings like output$source$['<doi>']")
}
