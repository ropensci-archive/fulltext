#' This function is defunct.
#' @export
#' @rdname ft_extract_corpus-defunct
#' @keywords internal
ft_extract_corpus <- function(...){
  .Defunct(msg = "function removed. see ?`fulltext-defunct`")
}

#' This function is defunct.
#' @export
#' @rdname pdfx-defunct
#' @keywords internal
pdfx <- function(...){
  .Defunct(msg = "function removed. see ?`fulltext-defunct`")
}

#' This function is defunct.
#' @export
#' @rdname chunks-defunct
#' @keywords internal
chunks <- function(...){
  .Defunct(new = "ft_chunks", package = "fulltext", msg = "function name changed to ft_chunks")
}

#' This function is defunct.
#' @export
#' @rdname tabularize-defunct
#' @keywords internal
tabularize <- function(...){
  .Defunct(new = "ft_tabularize", package = "fulltext", msg = "function name changed to ft_tabularize")
}

#' This function is defunct.
#' @export
#' @rdname collect-defunct
#' @keywords internal
collect <- function(...){
  .Defunct(new = "ft_collect", package = "fulltext", msg = "function name changed to ft_collect")
}

#' This function is defunct.
#' @export
#' @rdname get_text-defunct
#' @keywords internal
get_text <- function(...){
  .Defunct(new = "ft_text", package = "fulltext", msg = "function name changed to ft_text")
}

#' This function is defunct.
#' @export
#' @rdname ft_browse_sections-defunct
#' @keywords internal
ft_browse_sections <- function(...) {
  .Defunct(msg = "function removed. see ?`fulltext-defunct`")
}

#' This function is defunct.
#' @export
#' @rdname ft_get_si-defunct
#' @keywords internal
ft_get_si <- function(...) {
  .Defunct(msg = "function removed. see package suppdata")
}


#' Defunct functions in fulltext
#'
#'  - [ft_extract_corpus] Function removed. As part of focusing scope of the 
#'  package we're trying to limit dependencies, so downstream use of `tm` can 
#'  still easily be done.
#'  - [pdfx]: Function removed. We're trying to focus the scope of the 
#'  package - and this function is more out of scope now. 
#'  - [chunks]: Function name changed to [ft_chunks()]
#'  - [tabularize]: Function name changed to [ft_tabularize()]
#'  - [collect]: Function name changed to [ft_collect()]
#'  - [get_text]: Function name changed to [ft_text()]
#'  - `cache_clear` was never working anyway, and is now removed
#'  - [ft_browse_sections]: no sign that function used, and allows 
#'  to remove a dependency
#'  - [ft_get_si]: moved to package `suppdata`
#'  - [ft_chunks]: moved to package `pubchunks`
#'  - [ft_tabularize]: moved to package `pubchunks`
#' 
#' @name fulltext-defunct
NULL
