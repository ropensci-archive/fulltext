#' Microsoft Acadmic search
#' 
#' @export
#' @keywords internal
#' @param query (character) query terms
#' @param count (intger) number of records to return. default: 10
#' @param offset (intger) record to start at. default: 0
#' @param orderby (character) field to sort results by
#' @param atts (character) character vector of fields to return
#' @param key (character) microsoft academic API key, see Details.
#' @references 
#' https://academic.microsoft.com/
#' https://www.microsoft.com/cognitive-services/en-us/Academic-Knowledge-API/documentation/QueryExpressionSyntax
#' https://westus.dev.cognitive.microsoft.com/docs/services/56332331778daf02acc0a50b/operations/565d753be597ed16ac3ffc03
#' @examples \dontrun{
#' microsoft_search(query = "Ti='biology'...")
#' }
microsoft_search <- function(query, count = 10, offset = 0, orderby = NULL, 
  atts = c("Id", "AA.AuN", "J.JN", "Ti", "Y", "E", "CC"), key = NULL, ...) {
  
  if (!is.null(atts)) atts <- paste0(atts, collapse = ",")
  out <- ma_evaluate(query, count, offset, orderby, atts, key, ...)
  ee <- rbl(lapply(out$entities$E, function(z) {
    dat <- jsonlite::fromJSON(z)
    dat <- dat[names(dat) %in% c('DN', 'VFN', 'DOI', 'D')]
    data.frame(dat, stringsAsFactors = FALSE)
  }))
  out$entities$E <- NULL
  cbind(out$entities, ee)
}

# ma_evaluate("Ti='biology'...")
ma_evaluate <- function(query, count = 10, offset = 0, orderby = NULL, 
                        atts = "Id,AA.AuN,J.JN,Ti,Y,E,CC", key = NULL, ...) {
  
  args <- ft_compact(list(expr = query, count = count, offset = offset, 
               orderby = orderby, attributes = atts))
  ma_GET("evaluate", args, key, ...)
}

ma_interpret <- function(query, count = 10, complete = 1, key, ...) {
  args <- list(query = query, complete = complete, count = count, 
               model = "latest")
  ma_GET("interpret", args, key, ...)
}

ma_GET <- function(path, args, key, ...) {
  head <- httr::add_headers(
    `Ocp-Apim-Subscription-Key` = key
  )
  res <- httr::GET(file.path(ma_base(), path), query = args, head, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt)
}

ma_base <- function() "https://westus.api.cognitive.microsoft.com/academic/v1.0"
