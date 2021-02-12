ex <- function(str, pattern) regmatches(str, regexpr(pattern, str))
sm <- function(x) suppressMessages(x)
sw <- function(x) suppressWarnings(x)

# fetch ftdoi patterns before running tests
ftd_fetch_patterns()

library("vcr")
vcr::vcr_configure(
  dir = "../fixtures",
  write_disk_path = "../files",
  filter_sensitive_data = list(
    "<<crossref-email>>" = Sys.getenv("crossref_email"),
    "<<crossref-tdm-key>>" = Sys.getenv("CROSSREF_TDM"),
    "<<wiley-tdm-key>>" = Sys.getenv("WILEY_TDM_KEY"),
    "<<scopus-key>>" = Sys.getenv("ELSEVIER_SCOPUS_KEY"),
    "<<elsevier-tdm-key>>" = Sys.getenv("ELSEVIER_TDM_KEY"),
    "<<ma-key>>" = Sys.getenv("MICROSOFT_ACADEMIC_KEY"),
    "<<ncbi-key>>" = Sys.getenv("ENTREZ_KEY"),
    "<<springer-key>>" = Sys.getenv("SPRINGER_KEY")
  )
)
vcr::check_cassette_names()

# check if rcrossref API is down
has_crossref_api <- function() {
  url <- "https://api.crossref.org/works?rows=1"
  email <- Sys.getenv("crossref_email")
  if (nzchar(email)) {
    url <- paste0(url, "&mailto=", email)
  }
  crul::ok(url, timeout_ms=10000L)
}

skip_if_crossref_api_down <- function() {
  if (has_crossref_api()) return()
  testthat::skip("crossref API down or too slow")
}
