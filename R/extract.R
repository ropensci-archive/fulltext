#' Extract text from a single pdf document.
#' 
#' @export
#' @param path Path to a file
#' @param which One of rcamp, gs, or xpdf.
#' @param ... further args passed on
#' @return An object of class rcamp_char, gs_char, xpdf_char
#' @examples \donttest{
#' res <- extract(path, "rcamp")
#' res
#' res <- extract(path, "gs")
#' res
#' res <- extract(path, "xpdf")
#' res
#' }

extract <- function(path, which, ...){
  switch(which, 
         rcamp = extract_rcamp(path, ...),
         gs = extract_gs(path, ...),
         xpdf = extract_xpdf(path, ...)
  )
}

extract_rcamp <- function(path, which, ...){
  cmds <- list(...)
  cmds <- if(length(cmds)==0) "" else cmds
  path <- path.expand(path)
  res <- pdf_text(path)
  meta <- pdf_info(path)
  structure(list(meta=meta, data=res), class="rcamp_char", path=path)
}

extract_gs <- function(path, which, ...){
  cmds <- list(...)
  cmds <- if(length(cmds)==0) "" else cmds
  path <- path.expand(path)
  res <- pdf_text_via_gs(path)
  res <- paste(res, collapse = ", ")
  meta <- pdf_info_via_gs(path)
  structure(list(meta=meta, data=res), class="gs_char", path=path)
}

extract_xpdf <- function(path, which, ...){
  cmds <- list(...)
  cmds <- if(length(cmds)==0) "" else cmds
  path <- path.expand(path)
  res <- system2("pdftotext", shQuote(path), stdout = TRUE)
  res <- paste(res, collapse = ", ")
  meta <- pdf_info_via_xpdf(path)
  structure(list(meta=meta, data=res), class="xpdf_char", path=path)
}

#' @export
print.rcamp_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  File size: ", x$meta$`File Size`, "\n", sep = "")
  cat("  Pages: ", x$meta$`File Size`, "\n", sep = "")
  cat("  Producer: ", x$meta$Producer, "\n", sep = "")
  cat("  Creation date: ", x$meta$`File Size`, "\n", sep = "")
}

#' @export
print.gs_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Title: ", x$meta$Title, "\n", sep = "")
  cat("  Producer: ", x$meta$Producer, "\n", sep = "")
  cat("  Creation date: ", as.character(as.Date(x$meta$CreationDate)), "\n", sep = "")
}

#' @export
print.xpdf_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Pages: ", x$meta$Pages, "\n", sep = "")
  cat("  Title: ", x$meta$Title, "\n", sep = "")
  cat("  Producer: ", x$meta$Producer, "\n", sep = "")
  cat("  Creation date: ", as.character(as.Date(x$meta$CreationDate)), "\n", sep = "")
}
