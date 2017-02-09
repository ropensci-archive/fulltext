#' Scopus search
#' 
#' @export
#' @keywords internal
#' @references \url{}
#' @examples \dontrun{
#' scopus_search(query = "ecology")
#' }
scopus_search <- function(query = NULL, count = 25, type = "search", 
                          search_type = "scopus", key = NULL, ... ) {
  key <- check_key_scopus(key)
  args <- list(query = query, apiKey = key, count = count)
  res <- httr::GET(scopus_base(), query = args, ...)
  httr::stop_for_status(res)
  txt <- httr::content(res, "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

scopus_base <- function() "http://api.elsevier.com/content/search/scopus"

check_key_scopus <- function(x) {
  tmp <- if (is.null(x)) {
    Sys.getenv("ELSEVIER_SCOPUS_KEY", "")
  } else {
    x
  }
  if (tmp == "") {
    stop("you need an API key for the Scopus API",
         call. = FALSE)
  }
  return(tmp)
}
  # type = match.arg(type)
  # content_type = match.arg(content_type)
  # 
  # root_http = paste(root_http, content_type, sep = "/")
  # 
  # search_type = switch(type,
  #                      search = match.arg(search_type),
  #                      embase = "article",
  #                      serial = "title",
  #                      nonserial = "title",
  #                      entitlement = "entitlement",
  #                      holdings = "report.url",
  #                      "citation-count" = "citation-count",
  #                      citations = "citations"
  # )
  # if (type %in% c("entitlement","recommendation")) {
  #   type = "article"
  # }
  # if (type %in% c("citation-count", "citations")) {
  #   type = "abstract"
  # }
  # 
  # http = paste(type, search_type, sep = "/")
  # if (!is.null(http_end)) {
  #   http = paste0(http, http_end)
  # }
  # http = gsub("/$", "", http)
  # http = gsub("//", "/", http)
  # http = paste(root_http, http, sep = "/")
  # 
  # if (verbose){
  #   message(paste0("HTTP specified is:", http, "\n"))
  # }
  # if (!is.null(api_key)){
  #   qlist = list(
  #     "apiKey" = api_key,
  #     query = query,
  #     ...)
  # } else {
  #   qlist =  list(
  #     query = query, ...)
  # }
  # if (is.null(query)){
  #   qlist$query = NULL
  # }
  # if (length(qlist) > 0){
  #   r = GET(http,
  #           query = qlist,
  #           add_headers(headers)
  #   )
  # } else {
  #   r = GET(http,
  #           add_headers(headers)
  #   )
  # }
  # cr = content(r)
  # return(list(get_statement = r, content = cr))
#}

