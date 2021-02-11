# get plugins for ft_links input --------------------------------------

plugin_get_links_crossref <- function(from, urls, opts = list(), type, ...) {
  warn_crossref_tdm()
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
      tmp <- z[which(z$type == "unspecified"), "url"]
      if (length(tmp) > 1) {
        tmp <- z[which(z$type == "unspecified" & z$intended_application == "text-mining"), "url"]
      }
      out[[ names(urls)[i] ]] <- list(
        url = tmp,
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
  res <- list()
  for (i in seq_along(out)) {
    tdm <- as_ftdmurl(out[[i]]$url, type)
    lk <- tdm[[type]]
    if (is.null(lk)) {
      # res[[i]] <- ft_object(NULL, names(out)[i], type)
      res[[ names(out)[i] ]] <- NULL
    } else {
      path <- make_key(names(out)[i], type)
      if (file.exists(path) && !cache_options_get()$overwrite) {
        message(paste0("path exists: ", path))
        res[[ names(out)[i] ]] <- ft_object(path, names(out)[i], type)
      } else {
        cli <- crul::HttpClient$new(
          url = lk, 
          headers = list(
            Accept = paste0(switch(type, xml = "text/", pdf = "application/"), type)
          ),
          opts = list(followlocation = 1)
        )
        tmp <- cli$get(disk = path)
        if (tmp$status_code > 201) {
          unlink(path)
          res[[ names(out)[i] ]] <- NULL
        } else {
          res[[ names(out)[i] ]] <- ft_object(tmp$content, names(out)[i], type)
        }
      }
    }
  }
  
  list(
    found = length(res),
    dois = pluck(res, "id", ""), 
    data = list(
      backend = "ext",
      cache_path = cache_options_get()$path,
      path = stats::setNames(lapply(res, function(w) {
        list(
          path = w$path,
          id = w$id,
          type = w$type
        )
      }), pluck(res, "id", ""))
    ),
    opts = opts
  )
}

plugin_get_links_plos <- function(from, urls, opts = list(), type, ...) {
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
    path <- make_key(names(out)[i], type)
    if (file.exists(path) && !cache_options_get()$overwrite) {
      message(paste0("path exists: ", path))
      artout[[ names(out)[i] ]]$path <- path
    } else {
      tmp <- tryCatch(get_article(out[[i]]$url, path), error = function(e) e)
      artout[[ names(out)[i] ]]$path <- 
        if (inherits(tmp, "error")) NULL else path
    }
    artout[[ names(out)[i] ]]$id <- names(out)[i]
  }
  
  list(
    found = length(artout),
    dois = names(artout), 
    data = list(
      backend = "ext",
      cache_path = copts$path,
      path = stats::setNames(lapply(artout, function(w) {
        list(
          path = w$path,
          id = w$id,
          type = type
        )
      }), names(artout))
    ),
    opts = opts
  )
}

get_article <- function(x, path, ...) {
  cli <- crul::HttpClient$new(url = x, opts = list(...))
  res <- cli$get()
  if (res$status_code > 201) unlink(path)
  res$raise_for_status()
  res$content
}
