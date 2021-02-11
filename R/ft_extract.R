#' @title Extract text from a single pdf document
#' 
#' @description `ft_extract` attemps to make it easy to extract text from 
#' PDFs, using \pkg{pdftools}. Inputs can be either paths to PDF
#' files, or the output of [ft_get()] (class `ft_data`). 
#' 
#' @export
#' @param x Path to a pdf file, or an object of class `ft_data`, the 
#' output from [ft_get()]
#' @return An object of class `pdft_char` in the case of character input, 
#' or of class `ft_data` in the case of `ft_data` input
#' @examples \dontrun{
#' path <- system.file("examples", "example1.pdf", package = "fulltext")
#' (res <- ft_extract(path))
#' 
#' # use on output of ft_get() to extract pdf to text
#' ## arxiv
#' res <- ft_get('cond-mat/9309029', from = "arxiv")
#' res2 <- ft_extract(res)
#' res$arxiv$data
#' res2$arxiv$data
#' 
#' ## biorxiv
#' res <- ft_get('10.1101/012476')
#' res2 <- ft_extract(res)
#' res$biorxiv$data
#' res2$biorxiv$data
#' }
ft_extract <- function(x) {
  UseMethod("ft_extract")
}

#' @export
ft_extract.character <- function(x) {
  if (!file.exists(x)) stop("File does not exist", call. = FALSE)
  res <- .crm_extract(x)
  structure(list(meta = res$info, data = res$text), class = "pdft_char", 
            path = x)
}

#' @export
ft_extract.ft_data <- function(x) {
  do_extraction(x)
}

#' @export
ft_extract.raw <- function(x) {
  pdftools::pdf_text(x)
}

do_extraction <- function(x) {
  for (i in seq_along(x)) {
    path <- x[[i]]$data$path
    x[[i]]$data$data <- lapply(path, function(z) {
      ext <- strextract(z$path, "\\.[A-Za-z]{3}")
      w <- if (grepl("pdf", ext)) z$path else return(NULL)
      pdftools::pdf_text(w)
    })
  }
  return(x)
}

#' @export
print.pdft_char <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Title: ", x$meta$keys$Title, "\n", sep = "")
  cat("  Producer: ", x$meta$keys$Producer, "\n", sep = "")
  cat("  Creation date: ", as.character(as.Date(x$meta$created)), "\n", 
      sep = "")
}

.crm_extract <- function(path = NULL, raw = NULL, try_ocr = FALSE, ...) {
  assert(try_ocr, "logical")
  stopifnot(xor(is.null(path), is.null(raw)))
  if (!is.null(path)) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("path does not exist", call. = FALSE)  
  } else {
    assert(raw, "raw")
    path <- raw
  }
  fun <- if (try_ocr) pdftools::pdf_ocr_text else pdftools::pdf_text
  
  structure(
    list(
      info = pdftools::pdf_info(path, ...),
      text = fun(path, ...)
    ),
    class = "crm_pdf",
    path = if (is.character(path)) path else 'raw'
  )
}

#' @export
print.crm_pdf <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  Pages: ", x$info$pages, "\n", sep = "")
  cat("  No. characters: ", sum(nchar(x$text)), "\n", sep = "")
  cat("  Created: ", as.character(as.Date(x$info$created)), "\n",
      sep = "")
}
#' @export
print.crm_pdf_text <- function(x, ...) {
  cat("<document>", attr(x, "path"), "\n", sep = "")
  cat("  No. characters: ", sum(nchar(x$text)), "\n", sep = "")
}
