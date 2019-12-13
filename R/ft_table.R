#' @title Collect metadata and text into a data.frame 
#'
#' @description Facilitates downstream processing with text mining packages
#' by providing metadata and full text in a tidy data.frame format
#'
#' @export
#' @param path a directory path, must exist
#' @param type (character) type of files to get. Default is `NULL` which gets all types. 
#' Can be one of pdf, xml, or plain (file extensions: pdf, xml, and txt, respectively)
#' @param encoding (character) encoding, if `NULL` we get it from `getOption("encoding")`
#' @param xml_extract_text (logical) for XML, should we extract the text (`TRUE`) or 
#' return a string as XML (`FALSE`). Default: `TRUE`
#' @details You can alternatively use `readtext::readtext()` or similar functions
#' to achieve a similar outcome.
#' @examples  
#' \dontrun{
#' ## from a directory path
#' x <- ft_table()
#' x
#' 
#' ## only xml
#' ft_table(type = "xml")
#' 
#' ## only pdf
#' ft_table(type = "pdf")
#' 
#' ## don't pull text out of xml, just give back the xml please
#' x <- ft_table(xml_extract_text = FALSE)
#' x
#' }
ft_table <- function(path = NULL, type = NULL, encoding = NULL, xml_extract_text = TRUE) {
  if (is.null(path)) path <- cache_options_get()$path
  if (!is.character(path)) stop("'path' must be character class")
  if (!file.exists(path)) stop(path, " does not exist")
  if (is.null(encoding)) encoding <- getOption("encoding")
  pattern <- type
  if (!is.null(type)) {
    if (!type %in% c('pdf', 'xml')) stop("'type' must be one of 'xml', 'pdf', or 'plain'")
    pattern <- paste0(".", switch(type, xml = "xml", pdf = "pdf", plain = "txt"))
  }
  paths <- list.files(path, full.names = TRUE, pattern = pattern)
  out <- vapply(paths, reader, character(1), 
    encoding = encoding, xml_extract_text = xml_extract_text)
  doidf <- dois_lookup(gsub("\\..+", "", basename(paths)))
  tibble::as_tibble(data.frame(doidf, text = out, paths, stringsAsFactors = FALSE))
}

# helpers ---------------
check_read <- function(z, path) {
  if (inherits(z, "error")) {
    warning(path, " malformed, could not read", call. = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

reader <- function(x, encoding, xml_extract_text) {
  # switch reader based on file extension
  switch(
    sub("\\.", "", strextract(basename(x), "\\.[A-Za-z]+")),
    pdf = pdf_reader(x),
    xml = if (xml_extract_text) xml_reader(x, encoding) else txt_reader(x),
    txt = txt_reader(x)
  )
}

pdf_reader <- function(x) {
  tp <- tryCatch(pdftools::pdf_text(x), error = function(e) e)
  if (check_read(tp, x)) return(character(1))
  txt <- paste0(tp, collapse = "\n")
  Encoding(txt) <- "UTF-8"
  return(txt)
}

xml_reader <- function(x, encoding) {
  tp <- tryCatch(xml2::read_xml(x, encoding = encoding), error = function(e) e)
  if (check_read(tp, x)) return(character(1))
  xml2::xml_text(tp)
}

txt_reader <- function(x) {
  tmp <- paste0(readLines(cn <- file(x), warn = FALSE), collapse = "\n")
  close(cn)
  return(tmp)
}
