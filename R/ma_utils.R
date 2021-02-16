#' Microsoft Academic search
#'
#' Wraps `microdemic::ma_evaluate`
#'
#' @name microsoft-internals
#' @keywords internal
#' @param query (character) query terms
#' @param count (integer) number of records to return. default: 10
#' @param offset (integer) record to start at. default: 0
#' @param orderby (character) field to sort results by
#' @param atts (character) character vector of fields to return
#' @param key (character) microsoft academic API key, see
#' `Authentication` section in [fulltext-package]
#' @references
#' https://academic.microsoft.com/
#' @examples \dontrun{
#' microsoft_search2(query = "Y='19'...",
#'   key = Sys.getenv("MICROSOFT_ACADEMIC_KEY"))
#' }

#' @export
#' @rdname microsoft-internals
microsoft_search <- function(query, count = 10, offset = 0, orderby = NULL,
  atts = c('Id', 'DN', 'VFN', 'DOI', 'D'), key = NULL, ...) {

  out <- microdemic::ma_evaluate(query = query, count = count, offset = offset,
    orderby = orderby, atts = atts, key = key, ...)
  out$logprob <- NULL
  out$prob <- NULL
  return(out)
}

#' @export
#' @rdname microsoft-internals
microsoft_links <- function(query, count = 10, offset = 0, orderby = NULL,
  atts = c("Id", "AA.AuN", "J.JN", "Ti", "Y", "E", "CC"), key = NULL, ...) {

  out <- microdemic::ma_evaluate(query = query, count = count, offset = offset,
    orderby = orderby, atts = atts, key = key, ...)
  ee <- stats::setNames(lapply(out$E, function(z) {
    dat <- jsonlite::fromJSON(z)
    S <- dat$S
    if (is.null(S)) return(data.frame(NULL))
    names(S) <- c('type', 'url')
    dat
  }), out$Id)
  out$E <- NULL
  cbind(out, ee)
}
