biorxiv_search <- function(query, limit = 10, ...) {
  url <- file.path(bior_base(), URLencode(paste0(query, " numresults:30")))
  res <- GET(url)
  html <- xml2::read_html(content(res, "text"))
  found <- get_found(html)
  init_ret <- length(xml2::xml_find_all(html, "//*[contains(text(), 'dx.doi.org')]"))
  html_pages <- NULL
  if (init_ret < limit) {
    urls <- make_next_pages(min(c(found, limit)), init_ret, url)
    html_pages <- lapply(urls, function(z) {
      tmp <- GET(z)
      xml2::read_html(content(tmp, "text"))
    })
  }
  all_html <- unlist(list(list(html), html_pages), recursive = FALSE)
  res <- lapply(all_html, function(w) {
    href <- xml2::xml_find_all(w, "//a[contains(@href, 'biorxiv.org/content/early')]")
    hrefs <- vapply(href, function(y) xml_attr(y, "href"), "")
    doi <- xml2::xml_find_all(html, "//*[contains(text(), 'dx.doi.org')]")
    dois_url <- vapply(doi, function(y) strtrim(xml2::xml_text(xml2::xml_contents(y))[[2]]), "")
    dois <- gsub("http://dx.doi.org/", "", dois_url)
    suppressWarnings(rcrossref::cr_works(dois))$data
  })
  dat <- do.call(rbind, res)
  if (NROW(dat) > limit) dat <- dat[1:limit,]
  list(found = found, data = dat)
}

bior_base <- function() "http://www.biorxiv.org/search"

find_one_try <- function(html, x) {
  res <- tryCatch(suppressWarnings(xml2::xml_find_one(html, sprintf("//meta[@name='%s']", x))), error = function(e) e)
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
  tmp <- suppressWarnings(xml2::xml_find_one(x, '//div[@class="highwire-search-summary"]'))
  as.numeric(strextract(xml2::xml_text(tmp), "[0-9]+"))
}

make_next_pages <- function(x, y, url) {
  left <- x - y
  pgs <- 1:ceiling(left/30)
  paste0(paste0(url, "?page="), pgs)
}

# by_paper <- lapply(hrefs, function(g) {
#   html <- read_html(g)
#   lang <- find_one_try(html, 'DC.Language')
#   title <- find_one_try(html, 'DC.Title')
#   doi <- find_one_try(html, 'DC.Identifier')
#   date <- find_one_try(html, 'DC.Date')
#   publisher <- find_one_try(html, 'DC.Publisher')
#   authors <- paste0(find_all_try(html, 'DC.Contributor'), collapse = ", ")
#   email <- find_one_try(html, 'citation_author_email')
#   setNames(data.frame(lang, title, doi, date, publisher, authors, email, stringsAsFactors = FALSE),
#            c('language', 'title', 'doi', 'date', 'publsher', 'authors', 'email'))
# })
