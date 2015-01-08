#' Browse an article in your default browser.
#' 
#' @export
#' 
#' @param x An object of class \code{ft_data}
#' @param what (character) One of macrodocs (default), publisher, or whisker.
#' @param browse (logical) Whether to browse (default) or not. If \code{FALSE}, 
#' return the url.
#' 
#' @details 
#' \code{what=whisker} not operational yet. When operational, will use whisker to open 
#' html page from XML content, each section parsed into separate section. 
#' 
#' @examples \dontrun{
#' x <- ft_get(ids='10.7554/eLife.04300', from='elife')
#' x <- ft_get(ids='10.7554/eLife.04251', from='elife')
#' ft_browse(x)
#' ft_browse(x, browse=FALSE)
#' 
#' ft_browse( ft_get('10.3389/fphar.2014.00109', from="entrez") )
#' 
#' # open to publisher site
#' ft_browse(x, "publisher")
#' }

ft_browse <- function(x, what = "macrodocs", browse = TRUE) {
  what <- match.arg(what, c("macrodocs","publisher","whisker"))
  doi <- get_doi(x)
  url <- switch(what, 
                macrodocs = paste0(md(), doi),
                publisher = paste0(dx(), doi),
                whisker = stop("not working yet :)", call. = FALSE))
  if(browse) browseURL(url) else url
}

md <- function() "http://macrodocs.org/?doi="
dx <- function() "http://dx.doi.org/"

get_doi <- function(x){
  tmp <- ft_compact(sapply(x, function(v){
    tmp <- v$opts$doi
    if(is.null(tmp)) v$opts$ids else tmp
  }))[[1]]
  if(length(tmp) == 0) 
    stop("No DOIs found", call. = FALSE)
  else
    tmp
}


# xsltApplyStyleSheet(b, "~/Downloads/ViewNLM-v2.3/ViewNLM-v2.3.xsl")
