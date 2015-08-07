#' @title Extract text from a single pdf document
#' 
#' @description \code{ft_extract} attemps to make it easy to extract text from 
#' PDFs, using a variety of extraction tools. Inputs can be either paths to PDF
#' files, or the output of \code{\link{ft_get}} (class \code{ft_data}). 
#' 
#' @export
#' @param x Path to a pdf file, or an object of class \code{ft_data}, the 
#' output from \code{\link{ft_get}}
#' @param which One of gs or xpdf (default).
#' @param ... further args passed on
#' @return An object of class gs_char, xpdf_char
#' @details For xpdf, you can pass on addition options via flags. See Examples.
#' Right now, you can't pass options to Ghostscript if you're using the gs option.
#' 
#' xpdf installation: See \url{http://www.foolabs.com/xpdf/download.html} for 
#' instructions on how to download and install xpdf. For OSX, you an also get 
#' xpdf via homebrew.
#' 
#' ghostscript installation: See \url{http://www.ghostscript.com/doc/9.16/Install.htm} 
#' for instructions on how to download and install ghostscript
#' @examples \dontrun{
#' path <- system.file("examples", "example1.pdf", package = "fulltext")
#'
#' (res_xpdf <- ft_extract(path)) # xpdf is the default
#' (res_xpdf <- ft_extract(path, "xpdf"))
#' (res_gs <- ft_extract(path, "gs"))
#'
#' # pass on options to xpdf
#' ## preserve layout from pdf
#' ft_extract(path, "xpdf", "-layout")
#' ## preserve table structure as much as possible
#' ft_extract(path, "xpdf", "-table")
#' ## last page to convert is page 2
#' ft_extract(path, "xpdf", "-l 2")
#' ## first page to convert is page 3
#' ft_extract(path, "xpdf", "-f 3")
#' 
#' # use on output of ft_get() to extract pdf to text
#' ## arxiv
#' res <- ft_get('cond-mat/9309029', from = "arxiv")
#' res2 <- ft_extract(res)
#' res$arxiv$data
#' res2$arxiv$data
#' res2$arxiv$data$data[[1]]$data
#' 
#' ## biorxiv
#' res <- ft_get('10.1101/012476')
#' res2 <- ft_extract(res)
#' res$biorxiv$data
#' res2$biorxiv$data
#' res2$biorxiv$data$data[[1]]$data
#' }
ft_extract <- function(x, which = "xpdf", ...) {
  UseMethod("ft_extract")
}

#' @export
ft_extract.character <- function(x, which = "xpdf", ...) {
  which <- match.arg(which, c("gs", "xpdf"))
  if (!file.exists(x)) stop("File does not exist", call. = FALSE)
  switch(which,
         # rcamp = extract_rcamp(x, ...),
         gs = extract_gs(x, ...),
         xpdf = extract_xpdf(x, ...)
  )
}

#' @export
ft_extract.ft_data <- function(x, which = "xpdf", ...) {
  which <- match.arg(which, c("gs", "xpdf"))
  do_extraction(x, which, ...)
}

do_extraction <- function(x, which, ...) {
  lapply(x, function(y) {
    for (i in seq_along(y$data$path)) {
      y$data$data[[i]] <- 
        switch(which,
               gs = extract_gs(y$data$path[[i]], ...),
               xpdf = extract_xpdf(y$data$path[[i]], ...)
        )
    }
    y$data$data <- unclass(y$data$data)
    return( y )
  })
}

# extract_rcamp <- function(path, which, ...){
#   cmds <- get_cmds(...)
#   path <- path.expand(path)
#   res <- pdf_text(path)
#   meta <- pdf_info(path)
#   structure(list(meta=meta, data=res), class="rcamp_char", path=path)
# }

extract_gs <- function(path, ...){
  # cmds <- get_cmds(...)
  check_gs()
  path <- path.expand(path)
  res <- pdf_text_via_gs(path)
  res <- paste(res, collapse = ", ")
  meta <- pdf_info_via_gs(path)
  structure(list(meta = meta, data = res), class = "gs_char", path = path)
}

extract_xpdf <- function(path, ...){
  check_pdftotext()
  cmds <- get_cmds(...)
  path <- path.expand(path)
  res <- system2("pdftotext", paste(cmds, shQuote(path)))
  newpath <- sub("\\.pdf", ".txt", path)
  res <- paste(readLines(newpath, warn = FALSE), collapse = ", ")
  meta <- pdf_info_via_xpdf(path)
  structure(list(meta = meta, data = res), class = "xpdf_char", path = path)
}

check_pdftotext <- function(x) {
  chk <- Sys.which("pdftotext")
  if (chk == "") stop("Please install xpdf. See ?extract_tools for more", call. = FALSE)
}

check_gs <- function(x) {
  chk <- Sys.which("gs")
  if (chk == "") stop("Please install Ghostscript. See ?extract_tools for more", call. = FALSE)
}

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
  if (length(d) == 0) "" else paste0(unlist(d), collapse = " ")
}
