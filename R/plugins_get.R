# get plugins --------------------------------------

# cache helper --------------------------------------
construct_paths <- function(co, x, type){
  if (!co$cache) {
    list(backend = NULL,
         path = "session",
         data = x)
  } else {
    list(
      backend = co$backend,
      path = cache_save(obj = x, backend = co$backend, path = co$path),
      # path = if (type == "xml") {
      #   cache_save(obj = x, backend = co$backend, path = co$path)
      # } else {
      #   x
      # },
      data = NULL)
  }
}

pprint_cache <- function(x) {
  list(backend = NULL, path = x, data = NULL)
}

## plugin generator
plugin_get_generator <- function(srce, fun) {
  function(sources, ids, opts, type, ...) {
    if (any(grepl("plos", sources))) {
      ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
    }

    callopts <- list(...)
    if (any(grepl(eval(srce), sources))) {
      if (!any(grepl("arxiv", sources))) check_dois(ids)
      if (any(grepl("plos", sources))) {
        opts$doi <- ids
      } else if (any(sources %in% c("entrez", "crossref"))) {
        opts$ids <- ids
      } else {
        opts$dois <- ids
      }
      # if (
      #   any(sources %in% 
      #     c("arxiv", "biorxiv", "wiley", "scientificsocieties", "informa"))
      # ) opts$basepath <- ftxt_cache$cache_path_get()
      opts$basepath <- ftxt_cache$cache_path_get()
      opts <- c(opts, callopts)
      opts$type <- type
      out <- do.call(fun, opts)
      # deals with case where no results
      if (length(out) == 0) {
        return(list(found = NULL, dois = NULL, data = NULL, opts = opts))
      }
      attr(out, "format") <- type
      dat <- if (any(sources %in% c("arxiv", "biorxiv"))) {
        pprint_cache(out)
      } else {
        construct_paths(cache_options_get(), out, type)
      }
      list(found = length(out), dois = names(out), data = dat, opts = opts)
    } else {
      list(found = NULL, dois = NULL, data = NULL, opts = opts)
    }
  }
}

## make plugins
plugin_get_plos <- plugin_get_generator("plos", plos_fulltext)
plugin_get_bmc <- plugin_get_generator("bmc", bmc_ft)
plugin_get_elife <- plugin_get_generator("elife", elife_ft)
plugin_get_peerj <- plugin_get_generator("peerj", peerj_ft)
plugin_get_frontiersin <- plugin_get_generator("frontiersin", frontiersin_ft)
plugin_get_pensoft <- plugin_get_generator("pensoft", pensoft_ft)
plugin_get_copernicus <- plugin_get_generator("copernicus", copernicus_ft)
# plugin_get_cogent <- plugin_get_generator("cogent", cogent_ft)
plugin_get_crossref <- plugin_get_generator("crossref", cr_ft_xml)
plugin_get_entrez <- plugin_get_generator("entrez", entrez_get)
plugin_get_biorxiv <- plugin_get_generator("biorxiv", biorxiv_ft)
plugin_get_arxiv <- plugin_get_generator("arxiv", arxiv_ft)
plugin_get_elsevier <- plugin_get_generator("elsevier", elsevier_ft)
plugin_get_wiley <- plugin_get_generator("wiley", wiley_ft)
plugin_get_scientificsocieties <- plugin_get_generator("scientificsocieties", scientificsocieties_ft)
plugin_get_informa <- plugin_get_generator("informa", informa_ft)

## getters - could stand to make closure for the below as well, FIXME
# type: only xml
entrez_get <- function(ids, ...){
  res <- rentrez::entrez_search(db = "pmc",
                                term = paste0(sprintf('%s[doi]', ids),
                                              collapse = "|"), ...)
  if (length(res$ids) == 0) {
    res <- rentrez::entrez_search(db = "pubmed",
                                  term = paste0(sprintf('%s[doi]', ids),
                                                collapse = "|"), ...)
    tmp <- vapply(res$ids, function(z) {
      rentrez::entrez_fetch(db = 'pubmed', id = z, rettype = "xml", ...)
    }, character(1))
  } else {
    tmp <- vapply(res$ids, function(z) {
      rentrez::entrez_fetch(db = 'pmc', id = z, rettype = "xml", ...)
    }, character(1))
    if (length(tmp) != 0) {
      tmp <- stats::setNames(
        as.list(tmp),
        strsplit(gsub("\\[doi\\]", "", res$QueryTranslation), " OR ")[[1]]
      )
    }
  }
  structure(tmp, class = "entrez_ft")
}

#' @export
print.entrez_ft <- function(x, ...) {
  namesprint <- paste(stats::na.omit(names(x)[1:10]), collapse = " ")
  lengths <- vapply(x, nchar, 1, USE.NAMES = FALSE)
  cat(sprintf("%s full-text articles retrieved", length(x)),
      "\n")
  cat(sprintf("Min. Length: %s - Max. Length: %s", min(lengths),
              max(lengths)), "\n")
  cat(rplos:::rplos_wrap(sprintf("DOIs:\n %s ...", namesprint)), "\n\n")
  cat("NOTE: extract xml strings like output['<doi>']\n\n")
}

# type: ??
bmc_ft <- function(dois, ...) {
  stats::setNames(lapply(dois, function(x) {
    url <- sprintf("http://www.microbiomejournal.com/content/download/xml/%s.xml",
                   strextract(x, "[0-9-]+$"))
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# type: xml and pdf
elife_ft <- function(dois, basepath, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    lk <- tcat(crminer::crm_links(x, ...))
    lk <- tcat(Filter(function(x) grepl(paste0("\\.", type), x), lk)[[1]][[1]])
    if (inherits(lk, "error")) return(NULL)
    tmp <- httr::GET(lk, ...)
    if (type == "pdf") to_raw(tmp) else to_text(tmp)
  }), dois)
}

to_text <- function(x) httr::content(x, as = "text", encoding = "UTF-8")
to_raw <- function(x) httr::content(x, as = "raw")
tcat <- function(...) tryCatch(..., error = function(e) e)

# type: xml and pdf
peerj_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    url <- sprintf("https://peerj.com/articles/%s.xml", strextract(x, "[0-9]+$"))
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# type: xml and pdf
frontiersin_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    url <- sprintf("http://journal.frontiersin.org/article/%s/xml/nlm", x)
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# type: xml and pdf
pensoft_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    httr::content(httr::GET(rcrossref::cr_ft_links(x), ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# type: ??
copernicus_ft <- function(dois, ...) {
  stats::setNames(lapply(dois, function(x) {
    res <- HEAD(paste0("http://dx.doi.org/", x))
    url <- paste0(res$url, sub("10.5194/", "", x), ".xml")
    httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# cogent_ft <- function(dois, ...) {
#   lapply(dois, function(x) {
#     url <- paste0("http://cogentoa.tandfonline.com/doi/xml/", x)
#     httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
#   })
# }

# type: only pdf
arxiv_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    url <- sprintf("http://arxiv.org/pdf/%s.pdf", x)
    path <- file.path(basepath, sub("/", "_", sprintf("%s.pdf", x)))
    tmp <- httr::GET(url, httr::write_disk(path, TRUE), ...)
    tmp$request$output$path
  }), dois)
}

# type: only pdf
biorxiv_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    res <- HEAD(paste0("http://dx.doi.org/", x))
    url <- paste0(res$url, ".full.pdf")
    path <- file.path(basepath, sub("/", "_", sprintf("%s.pdf", x)))
    tmp <- httr::GET(url, httr::write_disk(path, TRUE), ...)
    tmp$request$output$path
  }), dois)
}

# type: pdf and xml
elsevier_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    res <- rcrossref::cr_works(dois = x)$data$link[[1]]
    url <- res[res$content.type == "text/xml", "URL"][[1]]
    header <- httr::add_headers(
      `CR-Clickthrough-Client-Token` = Sys.getenv("CROSSREF_TDM"),
      Accept = "text/xml"
    )
    httr::content(httr::GET(url, header, ...), as = "text", encoding = "UTF-8")
  }), dois)
}

# type: only pdf
wiley_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    res <- rcrossref::cr_works(dois = x)$data$link[[1]]
    url <- res[res$content.type == "unspecified", "URL"][[1]]
    header <- httr::add_headers(
      `CR-Clickthrough-Client-Token` = Sys.getenv("CROSSREF_TDM"),
      Accept = "application/pdf"
    )
    path <- file.path(basepath, 
      sprintf("%s.pdf", gsub("/|\\(|\\)|<|>|;|:|\\.", "_", x))
    )
    tmp <- httr::GET(url, header, httr::write_disk(path, TRUE), ...)
    tmp$request$output$path
  }), dois)
}

# type: only pdf
scientificsocieties_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    lk <- tryCatch(crminer::crm_links(x, ...)[[1]][[1]], error = function(e) e)
    if (inherits(lk, "error")) return(NULL)
    path <- file.path(basepath, sprintf("%s.pdf", gsub("/|\\(|\\)|<|>|;|:|\\.", "_", x)))
    tmp <- httr::GET(lk, httr::write_disk(path, TRUE), httr::config(followlocation = 1), ...)
    if (!grepl("application/pdf", tmp$headers$`content-type`)) {
      unlink(path)
      warning("you may not have access to ", x, " or an error occurred")
      return(NULL)
    }
    tmp$request$output$path
  }), dois)
}

# type: only pdf
informa_ft <- function(dois, basepath, ...) {
  stats::setNames(lapply(dois, function(x) {
    lk <- tryCatch(crminer::crm_links(x, ...)[[1]][[1]], error = function(e) e)
    if (inherits(lk, "error")) return(NULL)
    path <- file.path(basepath, sprintf("%s.pdf", gsub("/|\\(|\\)|<|>|;|:|\\.", "_", x)))
    tmp <- httr::GET(lk, httr::write_disk(path, TRUE), httr::config(followlocation = 1), ...)
    if (!grepl("application/pdf", tmp$headers$`content-type`)) {
      unlink(path)
      warning("you may not have access to ", x, " or an error occurred")
      return(NULL)
    }
    tmp$request$output$path
  }), dois)
}
