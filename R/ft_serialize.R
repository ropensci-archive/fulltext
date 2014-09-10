#' Serialize raw text to other formats, including to disk.
#' 
#' @export
#' 
#' @param x Input object, output from a call to \code{ft_get}. Required.
#' @param to (character) Format to serialize to. One of xml, json, etc. Required.
#' @param from (character) Format \code{x} is currently in. Function attempts to use metadata
#' provided, or guess from data itself. Optional. CURRENTLY IGNORED.
#' @param ... Further args passed on to \code{XML::xmlParse} or \code{jsonlite::toJSON}
#' @examples \donttest{
#' res <- ft_get(ids='10.1371%2Fjournal.pone.0086169', from='plos')
#' dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169')
#' res <- ft_get(ids=dois, from='plos')
#' ft_serialize(res, to='json')
#' ft_serialize(res)
#' }

ft_serialize <- function(x, to='xml', from=NULL, ...)
{
  fmt <- attributes(x$plos$data)$format
  tmp <- switch(to, 
                xml = to_xml(x, fmt, ...),
                json = to_json(x, fmt, ...))
  class(tmp) <- "ft_parsed"
  tmp
}

to_xml <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y) lapply(y$data, xmlParse))    
  } else {
    
  }
}

to_json <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y){ 
      y$data <- lapply(y$data, function(z){
        ztmp <- xmlToList(z)
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


#' Print brief summary of ft_parsed object
#'
#' @param x Input...
#' @param ... Ignored.
#' @method print ft_parsed
#' @export

print.ft_parsed <- function(x, ...) {
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data))))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(sapply(x, function(y) length(y$data)))
  lengths <- unlist( sapply(x, function(y){ if(!is.null(y$data)) vapply(y$data, nchar, 1) else NULL }) )
  cat(sprintf("[%s] full-text articles retrieved", totgot), "\n")
  cat(sprintf("Min. Length: %s - Max. Length: %s", min(lengths), max(lengths)), "\n")
  cat(ft_wrap(sprintf("IDs:\n %s ...", namesprint)), "\n\n")
  cat("NOTE: extract xml strings like output['<doi>']")
}
