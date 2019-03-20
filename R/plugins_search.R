plugin_search_plos <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("plos", sources))) {
    opts$q <- query
    opts$limit <- limit
    opts$callopts <- list(...)
    out <- do.call(searchplos, opts)
    zz <- list(source = "plos", found = out$meta$numFound, data = out$data,
      opts = opts,
      license = list(
        type = "CC-BY",
        uri = 'http://creativecommons.org/licenses/by/4.0/',
        text = '<authors> This is an open-access article distributed under
        the terms of the Creative Commons Attribution License, which permits
        unrestricted use, distribution, and reproduction in any medium,
        provided the original author and source are credited.'
      )
    )
    if (!inherits(zz$data, "data.frame")) zz$data <- data.frame(NULL)
    zz <- names_lower(zz)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "plos", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_crossref <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("crossref", sources))) {
    opts$query <- query
    opts$limit <- limit
    opts$offset <- start
    if ("filter" %in% names(opts)) {
      opts$filter <- c(c(has_license = TRUE), opts$filter)
    } else {
      opts$filter <- c(has_license = TRUE)  
    }
    opts <- c(opts, ...)
    out <- do.call(cr_works, opts)
    out$data <- names_lower(out$data)
    zz <- list(source = "crossref", found = out$meta$total_results, data = out$data, opts = opts,
               license = list(type = "variable, see individual records"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "crossref", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_bmc <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("bmc", sources))) {
    opts$query <- query
    opts$limit <- limit
    opts <- c(opts, ...)
    out <- do.call(bmc_search, opts)
    dat <- out$records
    dat <- names_lower(dat)
    opts$query <- NULL
    zz <- list(source = "bmc", found = out$result$total, data = dat, opts = opts, 
               license = list(type = "variable, see `openaccess` field in results"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "bmc", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_entrez <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("entrez", sources))) {
    opts$db <- "pmc"
    opts$term <- query
    opts$retmax <- limit
    out <- do.call(entrez_search, opts)
    sumres <- rentrez::entrez_summary(db = "pmc", id = out$ids, config = c(...))
    dat <- lapply(sumres, function(x) {
      x$authors <- paste(try_NULL(x$authors[,1]), collapse = ", ")
      x$pmid  <- try_NULL(x$articleids[x$articleids[,1] == "pmid", 2])
      x$doi   <- try_NULL(x$articleids[x$articleids[,1] == "doi",  2])
      x$pmcid <- try_NULL(x$articleids[x$articleids[,1] == "pmcid",2])
      x$mid   <- try_NULL(x$articleids[x$articleids[,1] == "MID",  2])
      x$articleids <- NULL
      lapply(x, function(z) if (is.null(z) | length(z) == 0) NA else z)
    })
    data <- do.call(rbind, lapply(dat, data.frame, stringsAsFactors = FALSE))
    row.names(data) <- NULL
    data <- move_col(data, "title")
    data <- move_col(data, "authors")
    data <- names_lower(data)

    zz <- list(source = "entrez", found = as.integer(out$count), data = data, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "entrez", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_europe_pmc <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("europmc", sources))) {
    opts$query <- query
    opts <- c(opts, ...)
    out <- do.call(eupmc_search, opts)
    zz <- list(source = "europmc", found = out$hitCount, 
               data = out$resultList$result, opts = opts,
               cursorMark = out$nextCursorMark)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "europmc", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_arxiv <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("arxiv", sources))) {
    opts$query <- query
    opts$limit <- limit
    out <- do.call(aRxiv::arxiv_search, opts)
    out <- names_lower(out)
    zz <- list(source = "arxiv", found = attributes(out)$total_results, data = out, opts = opts,
               license = list(type = "variable, but should be free to text-mine, see http://arxiv.org/help/license and http://arxiv.org/help/bulk_data"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "arxiv", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_biorxivr <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("biorxiv", sources))) {
    opts$query <- query
    opts$limit <- limit
    opts <- c(opts, ...)
    out <- do.call(biorxiv_search, opts)
    out$data <- names_lower(out$data)
    zz <- list(source = "biorxiv", found = out$found, data = out$data, opts = opts, 
               license = list(type = "variable, but free to text-mine, see http://biorxiv.org/about-biorxiv"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "biorxiv", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_scopus <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("scopus", sources))) {
    opts$query <- query
    opts$count <- limit
    opts$start <- start
    opts <- c(opts, ...)
    out <- do.call(scopus_search_loop, opts)
    df <- out$results
    df$`@_fa` <- df$link <- NULL
    zz <- list(source = "scopus", 
               found = out$found,
               data = df, 
               facets = out$facets,
               opts = opts)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "scopus", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_search_ma <- function(sources, query, limit, start, opts, ...){
  if (any(grepl("microsoft", sources))) {
    opts$query <- query
    opts$count <- limit
    opts$offset <- start
    opts <- c(opts, ...)
    out <- do.call(microsoft_search, opts)
    zz <- list(source = "microsoft", 
               found = NULL,
               data = out, 
               opts = opts)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(source = "microsoft", found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}
