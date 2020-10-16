# doi-crossref member
prefix_update <- function(doi, df) {
  storr_init("_doi_prefixes")
  ftdoi_doi_prefixes$set(
    key = doi_prefix(doi),
    value = list(prefix = doi_prefix(doi), member=df$member)
  )
}
prefix_get <- function(doi) {
  storr_init("_doi_prefixes")
  tryCatch(ftdoi_doi_prefixes$get(key = doi_prefix(doi)),
    error=function(e) e)
}
key_exists <- function(x) !inherits(x, "error") && (!is.null(x) && length(x) > 0) 
doi_prefix <- function(doi) strsplit(doi, "/")[[1]][1]
prefix_list_keys <- function() {
  storr_init("_doi_prefixes")
  ftdoi_doi_prefixes$list()
}

# doi-url
url_update <- function(doi, url, content_type) {
  storr_init("_urls")
  if (!nzchar(as.character(url)) || is.null(url)) return()
  if (!key_exists(url_get(doi, content_type))) {
    ftdoi_urls$set(
      key = paste(doi, content_type, sep = ":"),
      value = list(url=url, content_type=content_type)
    )
  }
}
url_get <- function(doi, content_type = NULL) {
  storr_init("_urls")
  if (is.null(content_type)) {
    keys <- grep(doi, ftdoi_urls$list(), value = TRUE)
    lapply(keys, function(z) ftdoi_urls$get(z))
  } else {
    tryCatch(ftdoi_urls$get(key = paste(doi, content_type, sep = ":")),
      error=function(e) e)
  }
}
url_exists <- function(doi) {
  storr_init("_urls")
  keys <- ftdoi_urls$list()
  any(grepl(doi, keys))
}
url_list_keys <- function() {
  storr_init("_urls")
  ftdoi_urls$list()
}

# doi-issn
doi_issn_update <- function(doi, issn) {
  storr_init("_doi_issn")
  if (!ftdoi_doi_issn$exists(doi)) {
    ftdoi_doi_issn$set(key = doi, value = issn)
  }
}
doi_issn_exists <- function(doi) ftdoi_doi_issn$exists(doi)
doi_issn_get <- function(doi) {
  storr_init("_doi_issn")
  tryCatch(ftdoi_doi_issn$get(key = doi), error=function(e) e)
}
doi_issn_list_keys <- function() {
  storr_init("_doi_issn")
  ftdoi_doi_issn$list()
}

# storr helper functions
storr_path <- function(x) file.path(ftdoi_cache$cache_path_get(), x)
storr_exists <- function(x) dir.exists(storr_path(x))
storr_create <- function(x) {
  storr::storr_rds(file.path(ftdoi_cache$cache_path_get(), x),
    mangle_key = TRUE)
}
storr_init <- function(x) {
  if (!storr_exists(x)) storr_create(x)
}

ftdoi_doi_prefixes <- ftdoi_urls <- ftdoi_doi_issn <- NULL
