#' Search for information on journals or publishers.
#' 
#' @export
#' 
#' @param journal Query terms
#' @param publisher Source to query
#' @param limit Number of records to return.
#' @param ... Further args passed on to \code{httr::GET}. Not working right now...
#' @return An object of class ft_p
#' 
#' @examples \dontrun{
#' # journal name search
#' ft_providers(journal="Stem Cells International")
#' ft_providers(publisher="hindawi")
#' ft_providers(publisher="journal")
#' }

ft_providers <- function(journal=NULL, publisher=NULL, limit=10, ...)
{
  if(!is.null(journal) && is.null(publisher)) res <- ft_journal(journal, ...)
  if(is.null(journal) && !is.null(publisher)) res <- ft_publisher(publisher, ...)
  structure(res, class="ft_p", journal=journal, publisher=publisher)
}

ft_journal <- function(x, ...){
  cr_journals(query = x, ...)
}

ft_publisher <- function(x, ...){
  cr_members(query = x, ...)
}
