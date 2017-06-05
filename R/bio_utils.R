#' Biorxiv search
#' 
#' @export
#' @keywords internal
#' @param query query terms
#' @param limit records to return. default: 10
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @examples \dontrun{
#' biorxiv_search(query = "ecology", config = verbose())
#' }
biorxiv_search <- function(query, limit = 10, ...) {
  url <- file.path(bior_base(), URLencode(paste0(query, " numresults:30")))
  res <- GET(url, ...)
  html <- xml2::read_html(content(res, "text", encoding = 'UTF-8'))
  found <- get_found(html)
  init_ret <- length(xml2::xml_find_all(html, "//*[contains(text(), 'doi.org')]"))
  html_pages <- NULL
  if (init_ret < limit) {
    urls <- make_next_pages(min(c(found, limit)), init_ret, url)
    html_pages <- lapply(urls, function(z) {
      tmp <- GET(z)
      xml2::read_html(content(tmp, "text", encoding = 'UTF-8'))
    })
  }
  all_html <- unlist(list(list(html), html_pages), recursive = FALSE)
  res <- lapply(all_html, function(w) {
    #href <- xml2::xml_find_all(w, "//a[contains(@href, '/content/early')]")
    #hrefs <- vapply(href, function(y) xml_attr(y, "href"), "")
    dois <- strtrim(strextract(
      xml2::xml_text(
        xml2::xml_find_all(
          w, 
          '//div[@class="highwire-cite-metadata"]//span[@class="highwire-cite-metadata-doi highwire-cite-metadata"]')
      ),
      "https.+"
    ))
    #doi <- xml2::xml_find_all(html, "//*[contains(text(), 'doi.org')]")
    #dois_url <- vapply(doi, function(y) strtrim(xml2::xml_text(xml2::xml_contents(y))[[2]]), "")
    #dois <- gsub("https://doi.org/", "", dois_url)
    suppressWarnings(rcrossref::cr_works(dois))$data
  })
  dat <- do.call(rbind, res)
  if (NROW(dat) > limit) dat <- dat[1:limit,]
  list(found = found, data = dat)
}

bior_base <- function() "http://www.biorxiv.org/search"

find_one_try <- function(html, x) {
  res <- tryCatch(suppressWarnings(xml2::xml_find_first(html, sprintf("//meta[@name='%s']", x))), error = function(e) e)
  if (is(res, "error")) {
    NA
  } else {
    xml_attr(res, "content")
  }
}

find_all_try <- function(html, x) {
  res <- tryCatch(suppressWarnings(xml2::xml_find_all(html, sprintf("//meta[@name='%s']", x))), error = function(e) e)
  if (is(res, "error")) {
    NA
  } else {
    xml_attr(res, "content")
  }
}

get_found <- function(x) {
  tmp <- suppressWarnings(xml2::xml_find_first(x, '//div[@class="highwire-search-summary"]'))
  as.numeric(
    gsub(
      ",",
      "",
      strextract(xml2::xml_text(tmp), "[0-9,]+")
    )
  )
}

make_next_pages <- function(x, y, url) {
  left <- x - y
  pgs <- 1:ceiling(left/30)
  paste0(paste0(url, "?page="), pgs)
}
