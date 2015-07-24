#' Search for information on journals or publishers.
#' 
#' @export
#' @importFrom rcrossref cr_journals cr_members
#' 
#' @param journal Query terms
#' @param publisher Source to query
#' @param limit Number of records to return.
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' 
#' @return An object of class ft_p
#' @examples \dontrun{
#' ft_providers(journal="Stem Cells International")
#' ft_providers(publisher="hindawi")
#' ft_providers(publisher="journal")
#' }

ft_providers <- function(journal=NULL, publisher=NULL, limit=10, ...) {
  if (!xor(!is.null(journal), !is.null(publisher))) {
    stop("Provide either journal or publisher", call. = FALSE)
  }
  if (!is.null(journal) && is.null(publisher)) res <- ft_journal(journal, ...)
  if (is.null(journal) && !is.null(publisher)) res <- ft_publisher(publisher, ...)
  structure(res, class = "ft_p", journal = journal, publisher = publisher)
}

ft_journal <- function(x, ...){
  cr_journals(query = x, ...)
}

ft_publisher <- function(x, ...){
  cr_members(query = x, ...)
}
