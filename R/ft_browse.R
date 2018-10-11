#' Browse an article in your default browser
#'
#' @name ft_browse
#' @param x An object of class `ft_data` - the output from a call to 
#' [ft_get()]
#' @param what (character) One of macrodocs (default) or publisher
#' @param browse (logical) Whether to browse (default) or not. If `FALSE`,
#' return the url.
#' @examples \dontrun{
#' x <- ft_get('10.7554/eLife.04300', from='elife')
#' ft_browse(x)
#' ft_browse(x, browse=FALSE)
#'
#' ft_browse( ft_get('10.3389/fphar.2014.00109', from="entrez") )
#'
#' # open to publisher site
#' ft_browse(x, "publisher")
#' }

#' @export
#' @rdname ft_browse
ft_browse <- function(x, what = "macrodocs", browse = TRUE) {
  what <- match.arg(what, c("macrodocs","publisher"))
  if (!inherits(x, "ft_data")) stop("x must be of class ft_data", call. = FALSE)
  doi <- get_doi(x)
  url <- switch(what,
                macrodocs = paste0(md(), doi),
                publisher = paste0(dx(), doi))
  if (browse) utils::browseURL(url) else url
}

md <- function() "http://macrodocs.org/?doi="
dx <- function() "https://doi.org/"

get_doi <- function(x){
  tmp <- ft_compact(sapply(x, function(v){
    tmp <- v$opts$doi
    if (is.null(tmp)) v$opts$ids else tmp
  }))[[1]]
  if (length(tmp) == 0) {
    stop("No DOIs found", call. = FALSE)
  } else {
    tmp
  }
}
