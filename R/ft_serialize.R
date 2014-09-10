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
#' # From XML to JSON
#' dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169')
#' res <- ft_get(ids=dois, from='plos')
#' (out <- ft_serialize(res, to='json'))
#' out$plos$data$`10.1371/journal.pone.0087376` # the json
#' jsonlite::fromJSON(out$plos$data$`10.1371/journal.pone.0087376`)
#' 
#' # Parse raw xml to XMLInternalDocument class
#' out <- ft_serialize(res, to='XMLInternalDocument')
#' out$plos$data$`10.1371/journal.pone.0087376`
#' 
#' # You can't go from JSON to XML
#' #    need example here...
#' }

ft_serialize <- function(x, to='XMLInternalDocument', from=NULL, ...)
{
  fmt <- attributes(x$plos$data)$format
  tmp <- switch(to, 
                XMLInternalDocument = to_xml(x, fmt, ...),
                json = to_json(x, fmt, ...))
  class(tmp) <- "ft_parsed"
  tmp
}

to_xml <- function(x, fmt, ...){
  fmt <- match.arg(fmt, c('xml','json'))
  if(fmt == 'xml'){
    lapply(x, function(y){ 
      y$data <- lapply(y$data, xmlParse)
      return( y )
    })
  } else {
    stop("No conversion from JSON to XML", call. = FALSE)
#     lapply(x, function(y){
#       y$data <- lapply(y$data, function(z){
#         ztmp <- jsonlite::fromJSON(z, FALSE)
#         listToXML(root, ztmp)
#       })
#       return( y )
#     })
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
#   lengths <- unlist( sapply(x, function(y){ if(!is.null(y$data)) vapply(y$data, nchar, 1) else NULL }) )
  cat(sprintf("[%s] documents parsed", totgot), "\n")
#   cat(sprintf("Min. Length: %s - Max. Length: %s", min(lengths), max(lengths)), "\n")
  cat(ft_wrap(sprintf("IDs:\n %s ...", namesprint)), "\n\n")
  cat("NOTE: extract xml strings like output$source$['<doi>']")
}
