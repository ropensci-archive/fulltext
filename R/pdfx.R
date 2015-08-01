#' PDF-to-XML conversion of scientific articles using pdfx
#'
#' Uses a web service provided by Utopia at \url{http://pdfx.cs.man.ac.uk/}.
#'
#' @import httr XML
#' @export
#'
#' @param file (character) Path to a file, or files on your machine.
#' @param what (character) One of parsed, text, or html.
#' @template config
#'
#' @author Scott Chamberlain {myrmecocystus@@gmail.com}
#' @return Raw XML text, parsed to XMLInternalDocument, or to html text
#'
#' @examples \dontrun{
#' path <- "~/github/sac/hovick_work/pdfs/Vaughn1937_Mauritius.pdf"
#' pdfx(file = path)
#' }

pdfx <- function(file = NULL, what = "parsed", ...)
{
  out <- pdfx_POST(file, ...)
  parsed <- xmlParse(out)
  meta <- pdfx_get_meta(parsed)

  toput <- switch(what,
                  parsed = XML::xmlParse(out),
                  text = out,
                  html = "not yet"
  )
  structure(list(meta=meta, data=toput), class="pdfx")
}

pdfx_POST <- function(file, ...) {
  url <- "http://pdfx.cs.man.ac.uk"
  res <- POST(url, config=c(content_type("application/pdf"), ...), body = upload_file(file))
  if(!res$status_code == 200) stop("something's wrong", call.=FALSE)
  stopifnot(res$headers$`content-type` == "text/xml")
  content(res, as = "text")
}

pdfx_GET <- function(input, type="html", write_path, ...) {
  type <- match.arg(type, c('html', "tar.gz"))
  stopifnot(is(input, "pdfx"))
  jobid <- input$meta$base_name
  url <- paste0(file.path("http://pdfx.cs.man.ac.uk", jobid), ".", type)
  if(type=="html"){
    res <- GET(url, ...)
    if(!res$status_code == 200) stop("something's wrong", call.=FALSE)
    content(res)
  } else {
    res <- GET(url, write_disk(path = write_path), ...)
    if(!res$status_code == 200) stop("something's wrong", call.=FALSE)
    message(sprintf('tar file written to\n   %s', write_path))
  }
}

pdfx_get_meta <- function(x){
  xpathApply(x, "//meta", xmlToList)[[1]]
}

#' Get html version of the extracted text
#'
#' @export
#' @param input Output from \code{pdfx} function
#' @template config
#' @examples \dontrun{
#' path <- "~/github/sac/scott/pdfs/BarraquandEtal2014peerj.pdf"
#' out <- pdfx(file = path)
#' pdfx_html(out)
#' }
pdfx_html <- function(input, ...) pdfx_GET(input, "html", ...)

#' Get tar.gz version of the extracted text
#'
#' @export
#' @param input Output from \code{pdfx} function
#' @param write_path Path to write tar ball to.
#' @template config
#' @examples \dontrun{
#' path <- "~/github/sac/scott/pdfs/BarraquandEtal2014peerj.pdf"
#' out <- pdfx(file = path)
#' tarfile <- tempfile(fileext = "tar.gz")
#' pdfx_targz(input = out, write_path = tarfile)
#' }
pdfx_targz <- function(input, write_path, ...) pdfx_GET(input, type = "tar.gz", write_path, ...)
