# docs: 
# - https://www.microsoft.com/cognitive-services/en-us/Academic-Knowledge-API/documentation/QueryExpressionSyntax
# - https://westus.dev.cognitive.microsoft.com/docs/services/56332331778daf02acc0a50b/operations/565d753be597ed16ac3ffc03

# microsoft_search(query = "Ti='biology'...")
microsoft_search <- function(query, count = 10, offset = 0, orderby = NULL, 
                             atts = "Id,AA.AuN,J.JN,Ti,Y,E,CC", 
                             key = NULL, ...) {
  
  out <- ma_evaluate(query, count, offset, orderby, atts, ...)
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
                        atts = "Id,AA.AuN,J.JN,Ti,Y,E,CC", ...) {
  
  args <- ft_compact(list(expr = query, count = count, offset = offset, 
               orderby = orderby, attributes = atts))
  ma_GET("evaluate", args, key, ...)
}

ma_interpret <- function(query, count = 10, complete = 1) {
  args <- list(query = query, complete = complete, count = limit, model = "latest")
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
