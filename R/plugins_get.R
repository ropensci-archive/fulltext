# get plugins --------------------------------------

## utilities
construct_paths <- function(co, x, type){
  if (!co$cache) {
    list(backend = NULL, path = "session", data = x)
  } else {
    list(
      backend = co$backend,
      cache_path = co$path,
      path = x,
      data = NULL
    )
  }
}

pprint_cache <- function(x) list(backend = NULL, path = x, data = NULL)

make_key <- function(id, type) {
  hash <- doi_normalize(id)
  file_ext <- get_file_ext(type)
  file.path(cache_options_get()$path, paste0(hash, file_ext))
}

null_list <- function(opts) {
  list(found = NULL, dois = NULL, data = NULL, opts = opts, 
    errors = data.frame(NULL))
}

ft_object <- function(path, id, type) {
  list(path = path, id = id, type = type, error = NULL)
}

ft_error <- function(error, id) {
  list(path = NULL, id = id, type = NULL, error = error)
}

tcat <- function(...) {
  tryCatch(..., error = function(e) e, warning = function(w) w)
}

get_ft <- function(x, type, url, path, headers = list(), ...) {
  cli <- crul::HttpClient$new(
    url = url, 
    opts = c(list(followlocation = 1, ...)),
    headers = headers
  )
  #cat(paste0("within get_ft: ", cli$url), sep="\n")
  res <- tryCatch(cli$get(disk = path), 
    error = function(e) e, 
    warning = function(w) w)
  #cat(class(res)[1L], sep = "\n")

  # if an error cleanup by deleting the file
  if (
    inherits(res, c("error", "warning")) ||  ## an error from tryCatch
    res$status_code > 201 || ## HTTP status code indicates an error
    !grepl(type, res$response_headers[['content-type']]) || ## content type does not match
    inherits(
      tryCatch(
        switch(type, 
          xml = xml2::read_xml(res$content), 
          pdf = pdftools::pdf_info(res$content)), 
      error=function(e) e), "error") ## invalid file, somehow gave 200 code
  ) {
    unlink(path)
    mssg <- if (inherits(res, c("error", "warning"))) {
      # tryCatch message
      res$message 
    } else if (
      !grepl(type, res$response_headers[['content-type']]) && 
      !is.null(res$response_headers[['content-type']])
    ) {
      # content type mismatch
      ct <- res$response_headers[['content-type']]
      sprintf("type was supposed to be `%s`, but was `%s`", type, ct)
    } else {
      # if all else fails just give a HTTP status code message back
      http_mssg(res)
    }
    warning("you may not have access to ", x, 
      "\n or an error occurred", 
      "\n or the downloaded file was invalid", 
      call. = FALSE)
    return(ft_error(mssg, x))
  }
  # if success return object
  return(ft_object(res$content, x, type))
}
http_mssg <- function(x) {
  b <- x$status_http()
  sprintf("(%s) %s", b$status_code, b$message)
}

# x = dat
error_df <- function(x) {
  tmp <- lapply(x$path, "[[", "error")
  data.frame(
    id = names(tmp), 
    error = unlist(Map(function(z) if (is.null(z)) NA_character_ else z, unname(tmp))), 
    stringsAsFactors = FALSE)
}

## plugin generator
plugin_get_generator <- function(srce, fun) {
  function(sources, ids, opts, type, url_pattern = NULL, ...) {
    if (any(grepl("plos", sources))) {
      ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
    }
    
    # do request
    ids <- stats::na.omit(ids)
    callopts <- list(...)
    if (any(grepl(eval(srce), sources))) {
      if (!any(grepl("arxiv", sources))) check_dois(ids)
      if (any(grepl("plos", sources))) {
        opts$doi <- ids
      } else if (any(sources %in% c("entrez"))) {
        opts$ids <- ids
      } else {
        opts$dois <- ids
      }

      opts <- c(opts, callopts)
      opts$type <- type
      if (!is.null(url_pattern)) opts$url_pattern <- url_pattern
      out <- do.call(fun, opts)
      
      # deals with case where no results
      if (length(out) == 0) return(null_list(opts))

      dat <- if (any(sources %in% c("arxiv", "biorxiv"))) {
        pprint_cache(out)
      } else {
        construct_paths(cache_options_get(), out, type)
      }
      list(found = length(ft_compact(lapply(out, "[[", "path"))), dois = names(out), 
        data = dat, opts = opts, errors = error_df(dat))
    } else {
      null_list(opts)
    }
  }
}

## make plugins
plugin_get_plos <- plugin_get_generator("plos", plos_wrapper)
plugin_get_bmc <- plugin_get_generator("bmc", bmc_ft)
plugin_get_elife <- plugin_get_generator("elife", elife_ft)
plugin_get_peerj <- plugin_get_generator("peerj", peerj_ft)
plugin_get_frontiersin <- plugin_get_generator("frontiersin", frontiersin_ft)
plugin_get_pensoft <- plugin_get_generator("pensoft", pensoft_ft)
plugin_get_copernicus <- plugin_get_generator("copernicus", copernicus_ft)
# plugin_get_cogent <- plugin_get_generator("cogent", cogent_ft)
#plugin_get_crossref <- plugin_get_generator("crossref", crminer::crm_xml)
plugin_get_entrez <- plugin_get_generator("entrez", entrez_ft)
plugin_get_biorxiv <- plugin_get_generator("biorxiv", biorxiv_ft)
plugin_get_arxiv <- plugin_get_generator("arxiv", arxiv_ft)
plugin_get_elsevier <- plugin_get_generator("elsevier", elsevier_ft)
plugin_get_wiley <- plugin_get_generator("wiley", wiley_ft)
plugin_get_scientificsocieties <- plugin_get_generator("scientificsocieties", scientificsocieties_ft)
plugin_get_informa <- plugin_get_generator("informa", informa_ft)
plugin_get_crossref <- plugin_get_generator("crossref", crossref_ft)
plugin_get_links <- plugin_get_generator("generic", got_link_ft)
plugin_get_royalsocchem <- plugin_get_generator("royalsocchem", roysocchem_ft)
plugin_get_ieee <- plugin_get_generator("ieee", ieee_ft)
plugin_get_aaas <- plugin_get_generator("aaas", aaas_ft)
plugin_get_pnas <- plugin_get_generator("pnas", pnas_ft)
plugin_get_microbiology <- plugin_get_generator("microbiology", microbiology_ft)
plugin_get_jama <- plugin_get_generator("jama", jama_ft)
plugin_get_amersocmicrobiol <- plugin_get_generator("amersocmicrobiol", amersocmicrobiol_ft)
plugin_get_amersocclinoncol <- plugin_get_generator("amersocclinoncol", amersocclinoncol_ft)
plugin_get_instinvestfil <- plugin_get_generator("instinvestfil", instinvestfil_ft)
plugin_get_aip <- plugin_get_generator("aip", aip_ft)


## getters - could stand to make closure for the below as well, FIXME
# plos - wrapper around rplos::plos_fulltext, via .plos_fulltext
plos_wrapper <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    tmp <- tryCatch(.plos_fulltext(x, disk = path, ...), 
      error = function(e) e, 
      warning = function(w) w)
    if (inherits(tmp, c("error", "warning"))) {
      unlink(path)
      mssg <- 'was not found or may be a DOI for a part of an article'
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    return(ft_object(path, x, type))
  }), dois)
}


# Entrez - wrapper around rentrez::entrez_search/rentrez::entrez_fetch
# type: only xml
entrez_ft <- function(ids, type = "xml", ...) {
  ids <- stats::na.omit(ids)
  db <- "pmc"
  if (length(ids) > 50) {
    chunk_size <- 50
    ids_chunked <- split(ids, ceiling(seq_along(ids)/chunk_size))
    out <- list()
    for (i in seq_along(ids_chunked)) {
      out[[i]] <- rentrez::entrez_search(
        db = db,
        term = paste0(sprintf('%s[doi]', ids_chunked[[i]]), collapse = "|")
      )
    }
    res <- list(ids = unlist(lapply(out, function(z) z$ids)))
  } else {
    res <- rentrez::entrez_search(
      db = db,
      term = paste0(sprintf('%s[doi]', ids), collapse = "|")
    )
  }
  
  if (length(res$ids) == 0) {
    db <- 'pubmed'
    if (length(ids) > 50) {
      chunk_size <- 50
      ids_chunked <- split(ids, ceiling(seq_along(ids)/chunk_size))
      out <- list()
      for (i in seq_along(ids_chunked)) {
        out[[i]] <- rentrez::entrez_search(
          db = db,
          term = paste0(sprintf('%s[doi]', ids_chunked[[i]]), collapse = "|")
        )
      }
      res <- list(ids = unlist(lapply(out, function(z) z$ids)))
    } else {
      res <- rentrez::entrez_search(
        db = db,
        term = paste0(sprintf('%s[doi]', ids), collapse = "|")
      )
    }
  }

  if (length(res$ids) == 0) return(NULL)
  stats::setNames(lapply(res$ids, function(z) {
    path <- make_key(z, 'xml')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, z, 'xml'))
    }
    # have to keep this httr usage
    invisible(rentrez::entrez_fetch(db = db, id = z, 
      rettype = "xml", config = httr::write_disk(path, cache_options_get()$overwrite)))
    ft_object(path, z, 'xml')
  }), res$ids)
}

# type: xml only presumably
bmc_ft <- function(dois, type = "xml", ...) {
  res <- rentrez::entrez_search(
    db = "pubmed", term = paste0(sprintf('%s[doi]', dois), collapse = "|"))
  if (length(res$ids) == 0) return(NULL)
  stats::setNames(lapply(res$ids, function(z) {
    path <- make_key(z, 'xml')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, z, 'xml'))
    }
    # have to keep this httr usage
    invisible(rentrez::entrez_fetch(db = 'pubmed', id = z, 
      rettype = "xml", config = httr::write_disk(path, cache_options_get()$overwrite)))
    ft_object(path, z, 'xml')
  }), dois)
}

# type: xml and pdf
elife_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    lk <- tcat(crminer::crm_links(x))
    lk <- tcat(Filter(function(x) grepl(paste0("\\.", type), x), lk)[[1]][[1]])
    if (inherits(lk, "error")) return(ft_error(lk$message, x))
    get_ft(x, type, lk, path, ...)
  }), dois)
}

# type: xml and pdf
peerj_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    url <- sprintf("https://peerj.com/articles/%s.%s", 
      strextract(x, "[0-9]+$"), type)
    get_ft(x, type, url, path, ...)
  }), dois)
}

# type: xml and pdf
frontiersin_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    url <- sprintf("https://www.frontiersin.org/articles/%s/%s", x, 
      if (type == "xml") "xml/nlm" else "pdf")
    get_ft(x, type, url, path, ...)
  }), dois)
}

# type: xml and pdf
pensoft_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    res <- tcat(crul::HttpClient$new(url = "https://ftdoi.org")$get(sprintf("api/doi/%s/", x)))
    if (inherits(res, c("error", "warning")) || !res$success()) {
      mssg <- if (inherits(res, c("error", "warning"))) res$message else http_mssg(res)
      return(ft_error(mssg, x))
    }
    lks <- jsonlite::fromJSON(res$parse("UTF-8"))$links
    url <- grep(type, lks$url, value = TRUE)
    get_ft(x, type, url, path, ...)
  }), dois)
}

# type: xml and pdf
copernicus_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    res <- tcat(crul::HttpClient$new(url = paste0("https://doi.org/", x))$head())
    if (inherits(res, c("error", "warning")) || !res$success()) {
      mssg <- if (inherits(res, c("error", "warning"))) res$message else http_mssg(res)
      return(ft_error(mssg, x))
    }
    url <- paste0(res$url, sub("10.5194/", "", x), ".", type)
    get_ft(x, type, url, path, ...)
  }), dois)
}

# cogent_ft <- function(dois, ...) {
#   lapply(dois, function(x) {
#     url <- paste0("http://cogentoa.tandfonline.com/doi/xml/", x)
#     httr::content(httr::GET(url, ...), as = "text", encoding = "UTF-8")
#   })
# }

# type: only pdf (type parameter is ignored)
arxiv_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }
    url <- sprintf("http://arxiv.org/pdf/%s.pdf", x)
    get_ft(x, 'pdf', url, path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
biorxiv_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    res <- tcat(crul::HttpClient$new(url = paste0("https://doi.org/", x))$head())
    if (inherits(res, c("error", "warning")) || !res$success()) {
      mssg <- if (inherits(res, c("error", "warning"))) res$message else http_mssg(res)
      return(ft_error(mssg, x))
    }
    url <- paste0(res$url, ".full.pdf")
    get_ft(x, 'pdf', url, path, ...)
  }), dois)
}

# type: plain and xml
elsevier_ft <- function(dois, type, ...) {
  if (!type %in% c('plain', 'xml')) stop("'type' for Elsevier must be 'plain' or 'xml'")
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    res <- tcat(rcrossref::cr_works(dois = x))
    if (inherits(res, c("error", "warning"))) return(ft_error(res$message, x))
    res <- res$data$link[[1]]
    url <- res[res$content.type == paste0("text/", type), "URL"][[1]]
    if (is.null(url)) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    header <- list(
      `CR-Clickthrough-Client-Token` = Sys.getenv("CROSSREF_TDM"),
      Accept = paste0(switch(type, xml = "text/", plain = "text/"), type)
    )
    get_ft(x, type, url, path, header, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
wiley_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }
    res <- tcat(rcrossref::cr_works(dois = x))
    if (inherits(res, c("error", "warning"))) return(ft_error(res$message, x))
    res <- res$data$link[[1]]
    url <- res[res$content.type == "unspecified", "URL"][[1]]
    if (is.null(url)) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    header <- list(
      `CR-Clickthrough-Client-Token` = Sys.getenv("CROSSREF_TDM"),
      Accept = "application/pdf"
    )
    get_ft(x, 'pdf', url, path, header, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
scientificsocieties_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tryCatch(crminer::crm_links(x), error = function(e) e, warning = function(w) w)
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    if (is.null(lk) || length(lk) == 0) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    get_ft(x, 'pdf', lk[[1]][[1]], path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
informa_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tryCatch(crminer::crm_links(x), error = function(e) e, warning = function(w) w)
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    if (is.null(lk) || length(lk) == 0) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    get_ft(x, 'pdf', lk[[1]][[1]], path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
roysocchem_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tryCatch(crminer::crm_links(x), error = function(e) e, warning = function(w) w)
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    if (is.null(lk) || length(lk) == 0) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    get_ft(x, 'pdf', lk[[1]][[1]], path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
ieee_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tryCatch(crminer::crm_links(x), error = function(e) e, warning = function(w) w)
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    if (is.null(lk) || length(lk) == 0) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    get_ft(x = x, type = 'pdf', url = lk[[1]][[1]], path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
aaas_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    url <- jsonlite::fromJSON(lk$parse("UTF-8"))$links$pdf
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
pnas_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    url <- jsonlite::fromJSON(lk$parse("UTF-8"))$links$pdf
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
microbiology_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    urls <- jsonlite::fromJSON(lk$parse("UTF-8"))$links
    url <- urls[grep("pdf", urls$`content-type`), "url"]
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
jama_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    urls <- jsonlite::fromJSON(lk$parse("UTF-8"))$links
    url <- urls[grep("pdf", urls$`content-type`), "url"]
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
amersocmicrobiol_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    urls <- jsonlite::fromJSON(lk$parse("UTF-8"))$links
    url <- urls[grep("pdf", urls$`content-type`), "url"]
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
amersocclinoncol_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    urls <- jsonlite::fromJSON(lk$parse("UTF-8"))$links
    url <- urls[grep("pdf", urls$`content-type`), "url"]
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
instinvestfil_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tryCatch(crminer::crm_links(x), error = function(e) e, warning = function(w) w)
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    if (is.null(lk) || length(lk) == 0) {
      mssg <- "has no link available"
      warning(x, " ", mssg, call. = FALSE)
      return(ft_error(mssg, x))
    }
    url = sub('view', 'download', lk[[1]][[1]])
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}

# type: only pdf (type parameter is ignored)
aip_ft <- function(dois, type = "pdf", ...) {
  stats::setNames(lapply(dois, function(x) {
    path <- make_key(x, 'pdf')
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, 'pdf'))
    }

    lk <- tcat(ftdoi_get(sprintf("api/doi/%s/", x)))
    if (inherits(lk, c("error", "warning"))) return(ft_error(lk$message, x))
    urls <- jsonlite::fromJSON(lk$parse("UTF-8"))$links
    url <- urls[grep("pdf", urls$`content-type`), "url"]
    get_ft(x = x, type = 'pdf', url = url, path = path, ...)
  }), dois)
}


# special Crossref plugin to try any DOI
crossref_ft <- function(dois, type, ...) {
  stats::setNames(lapply(dois, function(x) {
    lks <- tcat(crminer::crm_links(x))
    if (inherits(lks, c("error", "warning")) || inherits(lks, "warning")) {
      mssg <- lks$message
      if (inherits(lks, "error")) warning(x, " has no full text links available", call. = FALSE)
      if (inherits(lks, "warning")) warning(x, " not found", call. = FALSE)
      return(ft_error(mssg, x))
    }
    if (any(names(lks) == "unspecified")) {
      for (i in seq_along(lks)) {
        if (names(lks)[i] == "unspecified") {
          extype <- strextract(lks[[i]][[1]], "xml|pdf")
          if (length(extype) != 0) names(lks)[i] <- extype
        }
      }
    }
    lk <- lks[[type]]
    types <- c('xml', 'pdf')
    if (is.null(lk)) {
      lk <- lks[[ types[!types %in% type] ]]
      type <- types[!types %in% type]
      if (is.null(lk)) return(ft_error("no link found from Crossref", x))
    }

    # check if we already have it
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }

    # update path
    path <- sub("[A-Za-z]+$", type, path)

    # fetch it
    get_ft(x, type, lk[[1]], path, ...)
  }), dois)  
}

# special plugin when link already in hand
got_link_ft <- function(dois, type, url_pattern, ...) {
  stats::setNames(lapply(dois, function(x) {
    # try for the type passed, if not found, try for another
    types <- c('xml', 'pdf')
    pat <- url_pattern[[type]]
    if (is.null(pat)) {
      pat <- url_pattern[[ types[!types %in% type] ]]
      type <- types[!types %in% type]
      if (is.null(pat)) return(ft_error("no appropriate link found", x))
    }
    url <- sprintf(pat, x)
    path <- make_key(x, type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      return(ft_object(path, x, type))
    }
    get_ft(x, type, url, path, ...)
  }), dois)
}
