#' Coerce a url to a tdmurl with a specific type
#'
#' A tmd url is just a URL with some attributes to make it easier
#' to handle within other functions in this package.
#'
#' @export
#' @param url (character) A URL.
#' @param type (character) One of 'xml' (default), 'html', 'plain', 'pdf',
#' 'unspecified', or 'all'
#' @param doi (character) A DOI, optional, default: `NULL`
#' @param member (character) Crossref member id. optional
#' @param intended_application (character) intended application string, 
#' optional
#' @examples
#' as_ftdmurl("http://downloads.hindawi.com/journals/bmri/2014/201717.xml",
#'    "xml")
#' as_ftdmurl("http://downloads.hindawi.com/journals/bmri/2014/201717.pdf",
#'    "pdf")
#' out <-
#'  as_ftdmurl("http://downloads.hindawi.com/journals/bmri/2014/201717.pdf",
#'    "pdf", "10.1155/2014/201717")
#' attributes(out)
#' identical(attr(out, "type"), "pdf")
as_ftdmurl <- function(url, type, doi = NULL, member = NULL, 
  intended_application = NULL) {
  
  UseMethod("as_ftdmurl")
}

#' @export
#' @rdname as_ftdmurl
as_ftdmurl.ftdmurl <- function(url, type, doi = NULL, member = NULL, 
  intended_application = NULL) {
  
  return(url)
}

#' @export
#' @rdname as_ftdmurl
as_ftdmurl.character <- function(url, type, doi = NULL, 
  member = NULL, intended_application = NULL) {

  makeurl(check_url(url), type, doi, member, intended_application)
}

#' @export
print.ftdmurl <- function(x, ...) {
  cat("<url> ", x[[1]], "\n", sep = "")
}


# helpers --------
makeurl <- function(x, y, z, member, intended_application) {
  structure(stats::setNames(list(x), match_type(y)),
            class = "ftdmurl", type = match_type(y), doi = z,
            member = member, intended_application = intended_application)
}

check_url <- function(x) {
  if (!grepl("https?://", x)) stop("Not a proper url") else x
}

match_type <- function(x) {
  match.arg(x, c("xml","html","plain","pdf","unspecified","all"))
}
