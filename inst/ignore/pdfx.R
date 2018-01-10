#' @title PDF-to-XML conversion of scientific articles using pdfx
#'
#' @description Uses a web service provided by Utopia at 
#' \url{http://pdfx.cs.man.ac.uk/}. Beware, this can be quite slow. 
#' \code{pdfx} posts the pdf from your machine to the web service, 
#' \code{pdfx_html} takes the output of \code{pdfx} and gives back 
#' a html version of extracted text, and \code{pdfx_targz} 
#' gives a tar.gz version of the extracted text. This will not work
#' with PDFs that are scans of text, or mostly of images.
#'
#' @export
#' @param file (character) Path to a file, or files on your machine. Required.
#' @param what (character) One of parsed or text.
#' @template config
#' @param input Output from \code{pdfx} function
#' @param write_path Path to write tar ball to.
#' @return \code{pdfx} gives XML parsed to \code{xml_document}, \code{pdfx_html} 
#' gives html, \code{pdfx_targz} writes a tar.gz file to disk.
#' @examples \dontrun{
#' path <- system.file("examples", "example2.pdf", package = "fulltext")
#' pdfx(file = path)
#' 
#' out <- pdfx(file = path)
#' pdfx_html(out)
#' 
#' out <- pdfx(file = path)
#' tarfile <- tempfile(fileext = "tar.gz")
#' pdfx_targz(input = out, write_path = tarfile)
#' }
pdfx <- function(file, what = "parsed", ...) {
  out <- pdfx_POST(file, ...)
  parsed <- xml2::read_xml(out)
  meta <- pdfx_get_meta(parsed)

  toput <- switch(what,
                  parsed = xml2::read_xml(out),
                  text = out,
                  html = "not yet"
  )
  structure(list(meta = meta, data = toput), class = "pdfx")
}

#' @export
#' @rdname pdfx
pdfx_html <- function(input, ...) pdfx_GET(input, "html", ...)

#' @export
#' @rdname pdfx
pdfx_targz <- function(input, write_path, ...) pdfx_GET(input, type = "tar.gz", write_path, ...)

# Helper functions
pdfx_POST <- function(file, ...) {
  url <- "http://pdfx.cs.man.ac.uk"
  res <- POST(url, config = c(content_type("application/pdf"), ...), body = upload_file(file))
  pdfx_err(res)
  stopifnot(res$headers$`content-type` == "text/xml")
  content(res, as = "text", encoding = "UTF-8")
}

pdfx_err <- function(x) {
  stopifnot(is(x, "response"))
  if (!x$status_code == 200)  {
    xml <- content(x, "text", encoding = "UTF-8")
    stop(x$status_code, " - ", xml2::xml_text(xml2::read_xml(xml)), 
         call. = FALSE)
  }
}

pdfx_GET <- function(input, type="html", write_path, ...) {
  type <- match.arg(type, c('html', "tar.gz"))
  stopifnot(is(input, "pdfx"))
  jobid <- input$meta$base_name
  url <- paste0(file.path("http://pdfx.cs.man.ac.uk", jobid), ".", type)
  if (type == "html") {
    res <- GET(url, ...)
    if (!res$status_code == 200) stop("something's wrong", call. = FALSE)
    xml2::read_html(content(res, "text", encoding = "UTF-8"))
  } else {
    res <- GET(url, write_disk(path = write_path), ...)
    if (!res$status_code == 200) stop("something's wrong", call. = FALSE)
    message(sprintf('tar file written to\n   %s', write_path))
  }
}

pdfx_get_meta <- function(x) {
  sapply(xml2::xml_children(xml2::xml_find_all(x, "//meta")[[1]]), function(x) {
    as.list(setNames(xml2::xml_text(x), xml2::xml_name(x)))
  })
}
