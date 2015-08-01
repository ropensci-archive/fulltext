plugin_plos <- function(sources, query, limit, opts){
  if (any(grepl("plos", sources))) {
    opts$q <- query
    opts$limit <- limit
    out <- do.call(searchplos, opts)
    zz <- list(found = out$meta$numFound, data = out$data, opts = opts,
               license = list(type = "CC-BY",
                              uri = 'http://creativecommons.org/licenses/by/4.0/',
                              text = '<authors> This is an open-access article distributed under
                              the terms of the Creative Commons Attribution License, which permits
                              unrestricted use, distribution, and reproduction in any medium,
                              provided the original author and source are credited.'))
    if (!is(zz$data, "data.frame")) zz$data <- data.frame(NULL)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_crossref <- function(sources, query, limit, opts){
  if (any(grepl("crossref", sources))) {
    opts$query <- query
    opts$limit <- limit
    opts$filter <- c(has_license = TRUE)
    # opts$filter <- c(`license.url`='http://creativecommons.org/licenses/by/3.0/deed.en_US')
    out <- do.call(cr_works, opts)
    zz <- list(found = out$meta$total_results, data = out$data, opts = opts, 
               license = list(type = "variable, see individual records"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_bmc <- function(sources, query, limit, opts){
  if (any(grepl("bmc", sources))) {
    opts$terms <- query
    opts$limit <- limit
    out <- do.call(bmc_search, opts)
    dat <- do.call(rbind, lapply(out$results$entries, data.frame, stringsAsFactors = FALSE))
    opts$query <- opts$terms; opts$terms <- NULL
    zz <- list(found = NA, data = dat, opts = opts, 
               license = list(type = "variable, see `isOpenAccess` field in results"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_entrez <- function(sources, query, limit, opts){
  if (any(grepl("entrez", sources))) {
    opts$db <- "pmc"
    opts$term <- query
    opts$retmax <- limit
    out <- do.call(entrez_search, opts)
    sumres <- entrez_summary(db = "pmc", id = out$ids)
    dat <- lapply(sumres, function(x) {
      x$authors <- paste(x$authors[,1], collapse = ", ")
      x$pmid  <- x$articleids[x$articleids[,1] == "pmid", 2]
      x$doi   <- x$articleids[x$articleids[,1] == "doi",  2]
      x$pmcid <- x$articleids[x$articleids[,1] == "pmcid",2]
      x$mid   <- x$articleids[x$articleids[,1] == "MID",  2]
      x$articleids <- NULL
      lapply(x, function(z) if (is.null(z) | length(z) == 0) NA else z)
    })
    data <- do.call(rbind, lapply(dat, data.frame, stringsAsFactors = FALSE))
    row.names(data) <- NULL
    data <- move_col(data, "title")
    data <- move_col(data, "authors")

    zz <- list(found = as.integer(out$count), data = data, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_arxiv <- function(sources, query, limit, opts){
  if (any(grepl("arxiv", sources))) {
    opts$query <- query
    opts$limit <- limit
    out <- do.call(arxiv_search, opts)
    zz <- list(found = attributes(out)$total_results, data = out, opts = opts,
               license = list(type = "variable, but should be free to text-mine, see http://arxiv.org/help/license and http://arxiv.org/help/bulk_data"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }
}

plugin_biorxivr <- function(sources, query, limit, opts){
  if (any(grepl("biorxiv", sources))) {
    opts$query <- query
    opts$limit <- limit
    out <- do.call(biorxiv_search, opts)
    zz <- list(found = out$found, data = out$data, opts = opts, 
               license = list(type = "variable, but free to text-mine, see http://biorxiv.org/about-biorxiv"))
    structure(zz, class = "ft_ind", query = query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class = "ft_ind", query = query)
  }

}

move_col <- function(x, y) x[ c(names(x)[-grep(y, names(x))], y) ]
