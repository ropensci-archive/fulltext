#' Microsoft Academic search
#' 
#' Wraps `microdemic::ma_evaluate`
#'
#' @export
#' @keywords internal
#' @param query (character) query terms
#' @param count (integer) number of records to return. default: 10
#' @param offset (integer) record to start at. default: 0
#' @param orderby (character) field to sort results by
#' @param atts (character) character vector of fields to return
#' @param key (character) microsoft academic API key, see Details.
#' @references
#' https://academic.microsoft.com/
#' https://www.microsoft.com/cognitive-services/en-us/Academic-Knowledge-API/documentation/QueryExpressionSyntax
#' https://westus.dev.cognitive.microsoft.com/docs/services/56332331778daf02acc0a50b/operations/565d753be597ed16ac3ffc03
#' @examples \dontrun{
#' microsoft_search(query = "Y='19'...", key = Sys.getenv("MICROSOFT_ACADEMIC_KEY"))
#' }
microsoft_search <- function(query, count = 10, offset = 0, orderby = NULL,
  atts = c("Id", "AA.AuN", "J.JN", "Ti", "Y", "E", "CC"), key = NULL, ...) {

  out <- microdemic::ma_evaluate(query, count, offset, orderby, atts, key, ...)
  ee <- rbl(lapply(out$E, function(z) {
    dat <- jsonlite::fromJSON(z)
    dat <- dat[names(dat) %in% c('DN', 'VFN', 'DOI', 'D')]
    data.frame(dat, stringsAsFactors = FALSE)
  }))
  out$E <- NULL
  cbind(out, ee)
}
