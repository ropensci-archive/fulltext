#' Extract text from a single pdf document.
#' 
#' @export
#' @param path Path to a pdf file
#' @param which One of gs or xpdf (default).
#' @param x Input, printing
#' @param ... further args passed on
#' @return An object of class gs_char, xpdf_char
#' @examples \donttest{
#' path <- system.file("examples", "example1.pdf", package = "fulltext")
#' 
#' (res_xpdf <- ft_extract(path)) # xpdf is the default
#' (res_xpdf <- ft_extract(path, "xpdf"))
#' (res_gs <- ft_extract(path, "gs"))
#' }

ft_extract <- function(path, which = "xpdf", ...){
  which <- match.arg(which, c("gs", "xpdf"))
  if (!file.exists(path)) stop("File does not exist", call. = FALSE)
  switch(which, 
         # rcamp = extract_rcamp(path, ...),
         gs = extract_gs(path, ...),
         xpdf = extract_xpdf(path, ...)
  )
}

# extract_rcamp <- function(path, which, ...){
#   cmds <- get_cmds(...)
#   path <- path.expand(path)
#   res <- pdf_text(path)
#   meta <- pdf_info(path)
#   structure(list(meta=meta, data=res), class="rcamp_char", path=path)
# }

extract_gs <- function(path, which, ...){
  cmds <- get_cmds(...)
  path <- path.expand(path)
  res <- pdf_text_via_gs(path)
  res <- paste(res, collapse = ", ")
  meta <- pdf_info_via_gs(path)
  structure(list(meta = meta, data = res), class = "gs_char", path = path)
}

extract_xpdf <- function(path, which, ...){
  path <- path.expand(path)
  res <- system2("pdftotext", shQuote(path))
  newpath <- sub("\\.pdf", ".txt", path)
  res <- paste(readLines(newpath, warn = FALSE), collapse = ", ")
  meta <- pdf_info_via_xpdf(path)
  structure(list(meta = meta, data = res), class = "xpdf_char", path = path)
}

# #' @export
# print.rcamp_char <- function(x, ...) {
#   cat("<document>", attr(x, "path"), "\n", sep = "")
#   cat("  File size: ", x$meta$`File Size`, "\n", sep = "")
#   cat("  Pages: ", x$meta$`File Size`, "\n", sep = "")
#   cat("  Producer: ", x$meta$Producer, "\n", sep = "")
#   cat("  Creation date: ", x$meta$`File Size`, "\n", sep = "")
# }

#' @export
#' @rdname ft_extract
print.gs_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Title: ", x$meta$Title, "\n", sep = "")
  cat("  Producer: ", x$meta$Producer, "\n", sep = "")
  cat("  Creation date: ", as.character(as.Date(x$meta$CreationDate)), "\n", sep = "")
}

#' @export
#' @rdname ft_extract
print.xpdf_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Pages: ", x$meta$Pages, "\n", sep = "")
  cat("  Title: ", x$meta$Title, "\n", sep = "")
  cat("  Producer: ", x$meta$Producer, "\n", sep = "")
  cat("  Creation date: ", as.character(as.Date(x$meta$CreationDate)), "\n", sep = "")
}

get_cmds <- function(...){
  d <- list(...)
  if (length(d) == 0) "" else d
}
