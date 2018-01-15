#' Biorxiv search
#' 
#' @export
#' @keywords internal
#' @param query query terms
#' @param limit records to return. default: 10
#' @param date_from,date_to date begin and end, of form YYYY-MM-DD
#' @param ... curl options passed on to [crul::HttpClient]
#' @details We search Biorxiv first, get DOIs, then search Crossref - 
#' one consequence of this is that you may get back less than the number of 
#' results you requested even if Biorxiv found equal to or more than 
#' the amount you requested - BECAUSE we take the DOIs from the results and 
#' go out to Crossref to get what we think is better metadata than what
#' Biorxiv has.
#' @examples \dontrun{
#' biorxiv_search(query = "owls")
#' biorxiv_search(query = "owls", date_from = "2016-01-01", 
#'   date_to = "2016-12-30", limit = 10)
#' }
biorxiv_search <- function(query, limit = 10, date_from = NULL, 
                           date_to = NULL, ...) {
  
  url <- file.path(bior_base(), utils::URLencode(paste0(query, " numresults:30")))
  args <- ft_compact(list(limit_from = date_from, limit_to = date_to))
  cli <- crul::HttpClient$new(url = url, opts = list(...))
  res <- cli$get(query = args)
  html <- xml2::read_html(res$parse('UTF-8'))
  found <- get_found(html)
  if (length(found) == 0) stop("no results found in Biorxiv")
  init_ret <- length(xml2::xml_find_all(html, "//*[contains(text(), 'doi.org')]"))
  html_pages <- NULL
  if (init_ret < limit) {
    urls <- make_next_pages(min(c(found, limit)), init_ret, url)
    html_pages <- lapply(urls, function(z) {
      tmp <- crul::HttpClient$new(url = z)$get()
      xml2::read_html(tmp$parse('UTF-8'))
    })
  }
  all_html <- unlist(list(list(html), html_pages), recursive = FALSE)
  res <- lapply(all_html, function(w) {
    dois <- strtrim(strextract(
      xml2::xml_text(
        xml2::xml_find_all(
          w, 
          '//div[@class="highwire-cite-metadata"]//span[@class="highwire-cite-metadata-doi highwire-cite-metadata"]')
      ),
      "https.+"
    ))
    suppressWarnings(rcrossref::cr_works(dois))$data
  })
  dat <- do.call(rbind, res)
  if (NROW(dat) > limit) dat <- dat[1:limit,]
  list(found = found, data = dat)
}

bior_base <- function() "https://www.biorxiv.org/search"

find_one_try <- function(html, x) {
  res <- tryCatch(suppressWarnings(xml2::xml_find_first(html, sprintf("//meta[@name='%s']", x))), error = function(e) e)
  if (inherits(res, "error")) {
    NA
  } else {
    xml_attr(res, "content")
  }
}

find_all_try <- function(html, x) {
  res <- tryCatch(suppressWarnings(xml2::xml_find_all(html, sprintf("//meta[@name='%s']", x))), error = function(e) e)
  if (inherits(res, "error")) {
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
