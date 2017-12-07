# get plugins for ft_links input --------------------------------------

plugin_get_links_crossref <- function(from, urls, opts = list(), type, 
                                      cache, ...) {
  # pick URLs
  out <- list()
  for (i in seq_along(urls)) {
    z <- urls[[i]]
    if (NROW(z) == 0) {
      warning("no links for ", names(urls)[i])
      out[[ names(urls)[i] ]] <- NULL
    } else if (type %in% z$type) {
      out[[ names(urls)[i] ]] <- list(
        url = z[which(z$type == type), "url"],
        type = type,
        member = unique(z$member)
      )
    } else if ("unspecified" %in% z$type) { 
      out[[ names(urls)[i] ]] <- list(
        url = z[which(z$type == "unspecified"), "url"],
        type = "unspecified",
        member = unique(z$member)
      )
    } else {
      use <- if ("xml" %in% z$type) "xml" else z$type
      warning("no preferred or matching URL found for ", names(urls)[i], 
              " - using: ", use)
      out[[ names(urls)[i] ]] <- list(
        url = z[which(z$type == use), "url"],
        type = use,
        member = unique(z$member)
      )
    }
  }
  
  # remove NULLs
  out <- ft_compact(out)
  
  # fetch text
  res <- lapply(out, function(z) {
    attr(z$url, "member") <- z$member
    tmp <- tryCatch({
      tdm <- crminer::as_tdmurl(z$url, type)
      attr(tdm, "member") <- attr(z$url, "member")
      crminer::crm_text(url = tdm, type = type, 
        cache = cache, overwrite_unspecified = TRUE)
      },
      error = function(e) e
    )
    if (inherits(tmp, "error")) tmp$message else tmp
  })
  
  list(
    found = length(res),
    dois = names(res), 
    data = lapply(res, function(w) {
      list(
        backend = NULL,
        path = attr(w, "path"),
        data = tryCatch(w$text, error = function(e) NULL),
        error = if (inherits(tryCatch(w$text, error = function(e) e), "error")) {
          w
        } else {
          NULL
        }
      )
    }),
    # data = construct_paths(cache_options_get(), out),
    opts = opts
  )
}

plugin_get_links_plos <- function(from, urls, opts = list(), type, 
                                      cache, ...) {
  # pick URLs
  out <- list()
  for (i in seq_along(urls)) {
    z <- urls[[i]]
    if (length(z) == 0) {
      warning("no links for ", names(urls)[i])
      out[[ names(urls)[i] ]] <- NULL
    } else if (type %in% names(z)) {
      out[[ names(urls)[i] ]] <- list(
        url = z[[which(names(z) == type)]],
        type = type
      )
    } else {
      warning("no preferred or matching URL found for ", names(urls)[i], 
              " - using: ", names(z))
      out[[ names(urls)[i] ]] <- list(
        url = z[[1]],
        type = names(z)
      )
    }
  }
  
  # remove NULLs
  out <- ft_compact(out)
  
  # fetch text
  copts <- cache_options_get()
  artout <- list()
  for (i in seq_along(out)) {
    path <- NULL
    if (copts$cache) {
      path <- paste0(
        file.path(copts$path, gsub("/", "_", names(out)[i])),
        ".", out[[i]]$type
      )
    }
    tmp <- tryCatch(get_article(out[[i]]$url, path, out[[i]]$type), 
                    error = function(e) e)
    artout[[ names(out)[i] ]]$path <- 
      if (inherits(tmp, "error")) NULL else path
    artout[[ names(out)[i] ]]$text <- 
      if (inherits(tmp, "error")) tmp$message else tmp
  }
  
  list(
    found = length(artout),
    dois = names(artout), 
    data = lapply(artout, function(w) {
      list(
        backend = NULL,
        path = w$path,
        data = tryCatch(w$text, error = function(e) NULL),
        error = if (inherits(tryCatch(w$text, error = function(e) e), "error")) {
          w
        } else {
          NULL
        }
      )
    }),
    opts = opts
  )
}

get_article <- function(x, path, type, ...) {
  if (!is.null(path)) {
    res <- httr::GET(x, write_disk(path, TRUE), ...)
    httr::stop_for_status(res)
    res$request$output$path
  } else {
    switch(
      type,
      pdf = {
        tfile <- tempfile()
        res <- httr::GET(x, write_disk(tfile, TRUE))
        httr::stop_for_status(res)
        crminer::crm_extract(tfile)
      },
      xml = {
        res <- httr::GET(x, ...)
        httr::stop_for_status(res)
        httr::content(res, "text", encoding = "UTF-8")
      }
    )
  }
}

