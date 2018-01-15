#' @title Serialize raw text to other formats, including to disk
#' 
#' @description `ft_serialize` helps you convert to various data formats. If 
#' your data is in unparsed XML (i.e., character class), you can convert to 
#' parsed XML. If in XML, you can convert to (ugly-ish) JSON, or a list.
#'
#' @export
#' @param x Input object, output from a call to `ft_get`. Required.
#' @param to (character) Format to serialize to. One of list, 
#' xml, or json. Required. Output to xml returns object of 
#' class XMLInternalDocument.
#' @param from (character) Format `x` is currently in. Function attempts 
#' to use metadata provided, or guess from data itself. Optional. 
#' CURRENTLY IGNORED.
#' @param ... Further args passed on to [xml2::read_xml()] or
#' [jsonlite::toJSON()]
#' @return An object of class `ft_parsed`
#' @examples \dontrun{
#' res <- ft_get('10.7717/peerj.228')
#'
#' # if articles in xml format, parse the XML
#' (out <- ft_serialize(ft_collect(res), to='xml'))
#' out$peerj$data$data[[1]] # the xml
#'
#' # From XML to JSON
#' (out <- ft_serialize(ft_collect(res), to='json'))
#' out$peerj$data$data$`10.7717/peerj.228` # the json
#' jsonlite::fromJSON(out$peerj$data$data$`10.7717/peerj.228`)
#'
#' # To a list
#' out <- ft_serialize(ft_collect(res), to='list')
#' out$peerj$data$data
#' out$peerj$data$data[[1]]$body$sec$title
#' }

ft_serialize <- function(x, to = 'xml', from = NULL, ...) {
  is_or(x, c('ft_data', 'ft_parsed'))
  to <- match.arg(to, c('json','xml','list','file'))
  fmt <- attributes(x$plos$data$data)$format
  tmp <- switch(
    to,
    xml = to_xml(x, fmt, ...),
    json = to_json(x, fmt, ...),
    list = to_list(x, fmt, ...)
  )
  structure(tmp, class = "ft_parsed", type = to)
}

#' @export
print.ft_parsed <- function(x, ...) {
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data$data))))
  alldois <- vapply(alldois, utils::URLdecode, "")
  namesprint <- paste(stats::na.omit(alldois[1:10]), collapse = " ")
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
        dat <- y$data$data[[i]]
        y$data$data[[i]] <- if (!inherits(dat, "xml_document")) xml2::read_xml(dat) else dat
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
        # y$data$data[[i]] <- jsonlite::toJSON(xml2::as_list(xml2::read_xml(y$data$data[[i]])), ...)
        dat <- y$data$data[[i]]
        zz <- if (!inherits(dat, "xml_document")) xml2::read_xml(dat) else dat
        y$data$data[[i]] <- jsonlite::toJSON(xml2::as_list(zz))
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
        dat <- y$data$data[[i]]
        y$data$data[[i]] <- xml2::as_list(
          if (!inherits(dat, "xml_document")) xml2::read_xml(dat) else dat
        )
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

#' @export
#' @rdname ft_serialize
ft_get_keys <- function(x) {
  lapply(x, function(z) names(z$data))
}
