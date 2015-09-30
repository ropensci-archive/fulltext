# ft_links plugins
plugin_links_entrez <- function(sources, ids, opts, ...){
  if (any(grepl("entrez", sources))) {
    allids <- paste(paste0(ids, "[doi]"), collapse = " OR ")
    tmp <- entrez_search(db = "pubmed", term = allids)
    if (length(tmp$ids) == length(ids)) {
      message("Only ", length(tmp$ids), " found for Entrez, proceeding...")
    }
    res <- entrez_link(dbfrom = "pubmed", db = "", cmd = "llinks", id = tmp$ids)
    out <- lapply(res$linkouts, function(z) {
      rbind_fill(lapply(z, function(w) {
        w <- unclass(w)
        df <- data.frame(w, stringsAsFactors = FALSE)
        setNames(df, tolower(names(df)))
      }))
    })
    list(found = length(ft_compact(out)), ids = names(out), data = out, opts = opts)
  } else {
    list(found = NULL, ids = NULL,  data = NULL, opts = opts)
  }
}

plugin_links_plos <- function(sources, ids, opts, ...){
  if (any(grepl("plos", sources))) {
    ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
    tmp <- as.list(setNames(rplos::full_text_urls(ids), ids))
    tmp <- lapply(tmp, function(z) list(xml = z, pdf = sub("XML", "PDF", z)))
    list(found = length(tmp), ids = names(tmp), data = tmp, opts = opts)
  } else {
    list(found = NULL, ids = NULL,  data = NULL, opts = opts)
  }
}

plugin_links_bmc <- function(sources, ids, opts, ...){
  list(found = NULL, dois = NULL,  data = NULL, opts = opts)
}

plugin_links_crossref <- function(sources, ids, opts, ...){
  if (any(grepl("crossref", sources))) {
    tmp <- lapply(ids, cr_ft_links, type = "all")
    out <- lapply(tmp, function(z) {
      rbind_fill(lapply(z, function(w) {
        data.frame(list(url = w[1], doi = attr(w, "doi"), type = attr(w, "type")), stringsAsFactors = FALSE)
      }))
    })
    out <- ft_compact(out)
    out <- setNames(out, sapply(out, function(x) x$doi[1]))
    list(found = length(ft_compact(out)), ids = names(out), data = out, opts = opts)
  } else {
    list(found = NULL, ids = NULL,  data = NULL, opts = opts)
  }
}

plugin_links_arxiv <- function(sources, ids, opts, ...){
  list(found = NULL, dois = NULL,  data = NULL, opts = opts)
}

plugin_links_biorxiv <- function(sources, ids, opts, ...){
  list(found = NULL, dois = NULL,  data = NULL, opts = opts)
}
