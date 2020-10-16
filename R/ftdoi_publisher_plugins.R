# individual publiser methods
pub_pensoft <- function(doi, pat, member, issn, res = NULL) {
  if (!is.null(issn)) doi_issn_update(doi, issn)
  if (is.null(issn)) issn <- doi_issn_get(doi)
  lks <- make_links(doi,
    Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]$urls,
    pat$journals[[1]]$components$doi$regex)
  to_df(doi, pat, member, issn, lks)
}
fun_mdpi <- function(doi, pat, member, issn, res) {
  lks <- list()
  for (i in seq_along(pat$urls)) {
    lks[[i]] <- data.frame(
      url = sprintf(pat$urls[[i]], issn, res$volume, res$issue,
        strextract(doi, pat$components$doi$regex)),
      content_type = get_ctype(names(pat$urls)[i])
    )
  }
  return(rbl(lks))
}
fun_pnas <- function(doi, pat, member, issn, res) {
  lks <- list()
  for (i in seq_along(pat$urls)) {
    lks[[i]] <- data.frame(
      url = sprintf(pat$urls[[i]], res$volume, res$issue,
        sub("E", "", strsplit(res$page, "-")[[1]][1])),
      content_type = get_ctype(names(pat$urls)[i]),
      stringsAsFactors = FALSE
    )
  }
  return(rbl(lks))
}
fun_company_of_biologists <- function(doi, pat, member, issn, res) {
  z <- Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn),
    pat$journals)[[1]]
  data.frame(
    url = sprintf(z$urls$pdf, res$volume, res$issue, first_page(res$page)),
    content_type = get_ctype(names(z$urls)[1])
  )
}
fun_iif <- function(doi, pat, member, issn, res) {
  data.frame(
    url = sub("view", "download", res$link[[1]]$URL),
    content_type = get_ctype(names(pat$urls)[1])
  )
}
fun_hindawi <- function(doi, pat, member, issn, res) {
  links <- res$link[[1]]
  use <- links[links$content.type == "application/pdf",]
  data.frame(url = use$URL, content_type = use$content.type)
}
fun_aaas <- function(doi, pat, member, issn, res) {
  lks <- data.frame(url = fat_cat_link(doi), content_type = "application/pdf")
  if (is.na(lks$url)) {
    z <- Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]
    if (grepl("2375-2548", issn)) {
      last_part = strextract(doi, z$components$doi$regex, perl=TRUE)
    } else {
      last_part = strsplit(res$page, "-")[[1]][1]
    }
    url = sprintf(z$urls$pdf, res$volume, res$issue, last_part)
    lks <- data.frame(url = url, content_type = "application/pdf")
  }
  return(lks)
}
fun_cdc <- function(doi, pat, member, issn, res) {
  issn <- res$issn
  year <- strsplit(res$created, "-")[[1]][1]
  doi_part <- last(strsplit(last(strsplit(doi, '/')[[1]]), '\\.')[[1]])
  last_part <- paste(substring(doi_part, 1, 2),
    substring(doi_part, 3, nchar(doi_part)), sep="_")
  url <- sprintf(pat$urls$pdf, year, last_part)
  data.frame(url = url, content_type = "application/pdf")
}
fun_elsevier <- function(doi, pat, member, issn, res) {
  # FIXME: see alternative-id bit in pubpatternsapi
  make_links_no_regex(res$link[[1]]$URL, res$link[[1]]$content.type)
}
fun_american_society_for_microbiology <- function(doi, pat, member, issn, res) {
  issn <- res$issn
  url <- pat$urls$pdf
  vol <- res$volume
  iss <- res$issue
  journal_bit <- tolower(strextract(doi, "[A-Za-z]+"))
  other_bit <- strextract(strsplit(doi, "/")[[1]][2], "[0-9]+-[0-9]+")
  # if (length(other_bit) == 0) other_bit <- strextract(strsplit(doi, "/")[[1]][2], "[0-9]+$")
  if (length(other_bit) == 0) return(data.frame(url = NA_character_, content_type = "pdf"))
  lk <- sprintf(url, journal_bit, vol, iss, other_bit)
  data.frame(url = lk, content_type = "application/pdf")
}
fun_de_gruyter <- function(doi, pat, member, issn, res) {
  url <- res$link[[1]][res$link[[1]]$intended.application == "similarity-checking", ]$URL
  if (!grepl("view", url)) {
    url <- sub("\\.xml", "\\.pdf", url)
  } else {
    base <- strextract(dirname(url), "https?://[A-Za-z.]+")
    base <- file.path(base, "downloadpdf/journals")
    vol <- res$volume
    iss <- res$issue
    jabbrev <- strextract(last(strsplit(doi, "/")[[1]]), "[A-Za-z]+")
    page <- res$page
    if (is.null(page)) {
      url <- NA_character_
    } else {
      last_part <- sprintf("article-p%s.pdf", page)
      url <- file.path(base, jabbrev, vol, iss, last_part)
    }
  }
  data.frame(url = url, content_type = "application/pdf")
}
fun_biorxiv <- function(doi, pat, member, issn, res) {
  out <- crul::HttpClient$new(file.path("https://doi.org", doi),
    opts = list(followlocation=TRUE))$get()
  html <- xml2::read_html(out$parse("UTF-8"))
  url <- xml2::xml_attr(
    xml2::xml_find_all(html, '//meta[@name="citation_pdf_url"]'),
    "content")
  data.frame(url = url, content_type = "application/pdf")
}

# factories
pub_factory1 <- function() {
  function(doi, pat, member, issn, res=NULL) {
    lks <- make_links(doi, pat$urls, pat$components$doi$regex)
    to_df(doi, pat, member, issn, lks)
  }
}
pub_factory2 <- function() {
  function(doi, pat, member, issn, res=NULL) {
    if (!is.null(issn)) doi_issn_update(doi, issn)
    if (is.null(issn)) issn <- doi_issn_get(doi)
    lks <- make_links(doi,
      Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]$urls,
      pat$journals[[1]]$components$doi$regex)
    to_df(doi, pat, member, issn, lks)
  }
}
pub_factory3 <- function() {
  function(doi, pat, member, issn, res) {
    tmp <- res$link[[1]]
    lks <- list(
      url = tmp[tmp$intended.application == "similarity-checking", ]$URL,
      content_type = names(pat$urls)
    )
    url_update(doi, lks$url, lks$content_type %||% "")
    to_df(doi, pat, member, issn, lks)
  }
}
pub_factory4 <- function(fun_not_cached) {
  function(doi, pat, member, issn, res) {
    if (all("link" == names(res))) {
      lks <- links2df(res$link[[1]])
    } else {
      lks <- fun_not_cached(doi, pat, member, issn, res)
      for (i in seq_len(NROW(lks))) {
        url_update(doi, lks[i,]$url, lks[i,]$content_type %||% "")
      }
    }
    to_df(doi, pat, member, issn, lks)
  }
}

pub_frontiers <- pub_informa <- pub_emerald <-
  pub_pleiades <- pub_sage <- pub_spie <- pub_springer <- 
  pub_american_society_of_clinical_oncology <- pub_aip <- 
  pub_acs <- pub_the_royal_society <- pub_iop <- pub_factory1()
# pub_plos <- pub_thieme <- pub_peerj <- pub_factory2()
pub_plos <- pub_peerj <- pub_factory2()
pub_aps <- pub_rsc <- pub_karger <- pub_transtech <-
  pub_oxford <- pub_factory3()
pub_mdpi <- pub_factory4(fun_mdpi)
pub_pnas <- pub_factory4(fun_pnas)
pub_company_of_biologists <- pub_factory4(fun_company_of_biologists)
pub_iif <- pub_factory4(fun_iif)
pub_hindawi <- pub_factory4(fun_hindawi)
pub_aaas <- pub_factory4(fun_aaas)
pub_cdc <- pub_factory4(fun_cdc)
pub_elsevier <- pub_factory4(fun_elsevier)
pub_american_society_for_microbiology <- 
  pub_factory4(fun_american_society_for_microbiology)
pub_de_gruyter <- pub_factory4(fun_de_gruyter)
pub_biorxiv <- pub_factory4(fun_biorxiv)
