#' Get information on possibly bad files in your cache
#' 
#' @export
#' @family caching-functions
#' @return list, with three elements:
#' 
#' - xml_not_valid: xml files that could not be read in with 
#' `xml2::read_xml()`
#' - xml_abstract_only: xml files that only have abstracts. 
#' you can of choose to retain these if you like
#' - pdf_not_valid: pdf files that could not be read in with 
#' `pdftools::pdf_info()`
#' 
#' @details This function only identifies possibly bad files. 
#' You have to remove/delete them yourself. See example for 
#' how to do so. You can also open up your cache folder and 
#' delete them that way as well.
#' 
#' @examples
#' # identify likely bad files
#' res <- cache_file_info()
#' 
#' # you can remove them yourself, e.g.,
#' # invisible(lapply(res$xml_abstract_only, unlink))
cache_file_info <- function() {
  path <- cache_options_get()$path

  # XML
  xpths <- list.files(path, pattern = ".xml", full.names = TRUE)
  
  ## flag xml files that can't be read in
  message("flagging XML files that aren't valid ...")
  res <- vapply(xpths, function(z) {
    rr <- tryCatch(xml2::read_xml(z), error = function(e) e)
    if (inherits(rr, "error")) {
      unlink(z)
      return(FALSE)
    }
    TRUE
  }, logical(1))
  xml_not_valid <- names(which(!res))

  ## flag abstract only xml files that were downloaded 
  ## when not authorized with elsevier, or other publishers
  message("flagging XML files that are abstract only ...")
  res2 <- vapply(xpths, function(z) {
    xml <- xml2::read_xml(z)
    xml2::xml_ns_strip(xml)
    xml2::xml_length(xml2::xml_find_first(xml, "//body")) > 0
  }, logical(1))
  abstract_only <- names(which(!res2))

  # PDF
  ppths <- list.files(path, pattern = ".pdf", full.names = TRUE)

  ## flag pdf files that seem not valid
  message("flagging PDF files that aren't valid ...")
  res3 <- vapply(ppths, function(z) {
    rr <- tryCatch(pdftools::pdf_info(z), error = function(e) e)
    if (inherits(rr, "error")) {
      unlink(z)
      return(FALSE)
    }
    TRUE
  }, logical(1))
  pdf_not_valid <- names(which(!res3))

  # return
  list(
    xml_not_valid = xml_not_valid, 
    xml_abstract_only = abstract_only,
    pdf_not_valid = pdf_not_valid
  )
}
