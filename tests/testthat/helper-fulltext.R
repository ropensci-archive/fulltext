ex <- function(str, pattern) regmatches(str, regexpr(pattern, str))
sm <- function(x) suppressMessages(x)
sw <- function(x) suppressWarnings(x)

library("vcr")
vcr::vcr_configure(
  dir = "../fixtures",
  filter_sensitive_data = list(
    "<<crossref-email>>" = Sys.getenv("crossref_email"),
    "<<crossref-tdm-key>>" = Sys.getenv("CROSSREF_TDM"),
    "<<scopus-key>>" = Sys.getenv("ELSEVIER_SCOPUS_KEY"),
    "<<elsevier-tdm-key>>" = Sys.getenv("ELSEVIER_TDM_KEY"),
    "<<ma-key>>" = Sys.getenv("MICROSOFT_ACADEMIC_KEY"),
    "<<ncbi-key>>" = Sys.getenv("ENTREZ_KEY"),
    "<<springer-key>>" = Sys.getenv("SPRINGER_KEY")
  )
)
vcr::check_cassette_names()
