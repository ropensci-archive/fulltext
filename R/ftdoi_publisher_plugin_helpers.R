members_need_crossref <- c(
  "16", "292", "127", "2457", "341", "237",
  "1968", "8215", "98", "221", "286", "1822",
  "78", "235", "374"
)
members_need_issn <- c("2258", "340", "194", "4443")
members_sim_check <- c("16", "292", "127", "2457", "286")
pattern_path <- function(id) {
  file.path(ftdoi_cache$cache_path_get(), "patterns", member_map[[id]]$path)
}


mem_list <- list(
  "2258" = "pensoft",
  "340" = "plos",
  "1968" = "mdpi",
  "1965" = "frontiers",
  "301" = "informa",
  "194" = "thieme",
  "4443" = "peerj",
  "16" = "aps",
  "292" = "rsc",
  "127" = "karger",
  "2457" = "transtech",
  "140" = "emerald",
  "137" = "pleiades",
  "8215" = "iif",
  "179" = "sage",
  "189" = "spie",
  "341" = "pnas",
  "297" = "springer",
  "233" = "american_society_of_clinical_oncology",
  "317" = "aip",
  "316" = "acs",
  "175" = "the_royal_society",
  "237" = "company_of_biologists",
  "98" = "hindawi",
  "266" = "iop",
  "221" = "aaas",
  "286" = "oxford",
  "1822" = "cdc",
  "78" = "elsevier",
  "235" = "american_society_for_microbiology",
  "374" = "de_gruyter",
  "246" = "biorxiv"
)
pattern_member <- function(doi, member, issn, res = NULL) {
  pub <- mem_list[[member]]
  pat <- jsonlite::fromJSON(pattern_path(member), FALSE)
  fun <- eval(parse(text=paste0("pub_", pub)))
  fun(doi, pat, member, issn, res)
}

get_ctype <- function(x) {
  switch(x,
    pdf = 'application/pdf',
    xml = 'application/xml',
    html = 'text/html'
  )
}
murl <- function(x) file.path("https://api.crossref.org/members", x)
iurl <- function(x) {
  if (is.null(x) || !nzchar(as.character(x))) return(NA_character_)
  x <- strsplit(x, ",")[[1]][1]
  file.path("https://api.crossref.org/journals", x)
}
make_links <- function(doi, z, regex) {
  out = list()
  for (i in seq_along(z)) {
    out[[i]] <- data.frame(
      url = sprintf(z[[i]], strextract(doi, regex, perl=TRUE)),
      content_type = get_ctype(names(z)[i]),
      stringsAsFactors = FALSE
    )
  }
  return(do.call(rbind, out))
}
make_links_no_regex <- function(urls, ctypes) {
  stopifnot(length(urls) == length(ctypes))
  out = list()
  for (i in seq_along(urls)) {
    out[[i]] <- list(url = urls[i], `content-type` = ctypes[i])
  }
  return(out)
}
have_pattern <- function(member) {
  if (is.null(mem_list[[member]])) {
    stop("member ", member, " is not supported\n",
    "  open an issue https://github.com/ropensci/ftdoi/issues",
    call. = FALSE)
  }
}
