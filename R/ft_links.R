#' @title Get full text links
#'
#' @export
#' @param x Query terms
#' @param from Source to query
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param crossrefopts Crossref options. See \code{?cr_works}
#' @param entrezopts Entrez options. See \code{?entrez_search}
#' @param arxivopts arxiv options. See \code{?arxiv_search}
#' @param biorxivopts biorxiv options. See \code{?bx_search}
#' @param ... Further args passed on to \code{\link[httr]{GET}}. Not working right now...
#'
#' @return An object of class ft_links
#'
#' @examples \dontrun{
#' # Entrez
#' (res1 <- ft_search(query='ecology', from='entrez'))
#' res1$entrez$data$doi
#' (out <- ft_links(res1))
#' out$entrez
#' out$entrez$data[[1]]
#' 
#' # Crossref
#' (res2 <- ft_search(query='ecology', from='crossref'))
#' res2$crossref$data$doi
#' (out <- ft_links(res2))
#' out$crossref
#' out$crossref$data[[1]]
#' 
#' # PLOS
#' (res3 <- ft_search(query='ecology', from='plos', plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all','alm_twitterCount'))))
#' res3$plos$data$id
#' (out <- ft_links(res3))
#' out$plos
#' out$plos$data[[1]]
#' 
#' # Character input
#' ft_links('10.1371/journal.pone.0086169')
#' }
ft_links <- function(x, from = NULL, plosopts = list(), bmcopts = list(), crossrefopts = list(),
                    entrezopts = list(), arxivopts = list(), biorxivopts = list(), ...) {
  UseMethod("ft_links")
}

#' @export
#' @rdname ft_links
ft_links.ft <- function(x, from = NULL, 
                        plosopts = list(),
                        bmcopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        arxivopts = list(),
                        biorxivopts = list(),
                        ...) {
  
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  plos_out <- plugin_links_plos(from, x$plos$data$id, plosopts)
  bmc_out <- plugin_links_bmc(from, x, bmcopts)
  cr_out <- plugin_links_crossref(from, x$crossref$data$doi, crossrefopts)
  en_out <- plugin_links_entrez(from, x$entrez$data$doi, entrezopts)
  arx_out <- plugin_links_arxiv(from, x, arxivopts)
  bio_out <- plugin_links_biorxiv(from, x, biorxivopts)
  
  res <- list(plos = plos_out, bmc = bmc_out, crossref = cr_out,
              entrez = en_out, arxiv = arx_out, biorxiv = bio_out)
  structure(res, class = "ft_links")
}

#' @export
#' @rdname ft_links
ft_links.character <- function(x, from = NULL, 
                        plosopts = list(),
                        bmcopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        arxivopts = list(),
                        biorxivopts = list(),
                        ...) {
  
  if (!is.null(from)) {
    from <- names(x[sapply(x, function(v) !is.null(v$data))])
    plos_out <- plugin_links_plos(from, x$plos$data$id, plosopts)
    bmc_out <- plugin_links_bmc(from, x, bmcopts)
    cr_out <- plugin_links_crossref(from, x$crossref$data$doi, crossrefopts)
    en_out <- plugin_links_entrez(from, x$entrez$data$doi, entrezopts)
    arx_out <- plugin_links_arxiv(from, x, arxivopts)
    bio_out <- plugin_links_biorxiv(from, x, biorxivopts)
    
    res <- list(plos = plos_out, bmc = bmc_out, crossref = cr_out,
                entrez = en_out, arxiv = arx_out, biorxiv = bio_out)
    structure(res, class = "ft_links")
  } else {
    get_unknown_links(x, ...)
  }
}

#' @export
print.ft_links <- function(x, ...) {
  cat("<fulltext links>", sep = "\n")
  alldois <- unlist(ft_compact(pluck(x, "ids")))
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(unlist(pluck(x, "found")))
  cat(sprintf("[Found] %s", totgot), "\n")
  cat(ft_wrap(sprintf("[IDs]\n %s ...", namesprint)), "\n\n")
}

# get unknown from DOIs where from=NULL ------------------
get_unknown_links <- function(x, ...) {
  pubs <- ft_compact(sapply(x, get_publisher))
  df <- data.frame(pub = unlist(unname(pubs)), doi = names(pubs), stringsAsFactors = FALSE)
  dfsplit <- split(df, df$pub)
  out <- list()
  for (i in seq_along(dfsplit)) {
    fun <- publisher_plugin_links(names(dfsplit)[i])
    pub_nm <- get_pub_name(names(dfsplit)[i])
    tm_nm <- get_tm_name(names(dfsplit)[i])
    out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), ...)
  }
  structure(out, class = "ft_links")
}

publisher_plugin_links <- function(x) {
  switch(x, 
         `4374` = plugin_links_elife,
         `340` = plugin_links_plos,
         `4443` = plugin_links_peerj,
         `297` = plugin_links_bmc,
         `1965` = plugin_links_frontiersin,
         `98` = plugin_links_entrez,
         `4950` = plugin_links_entrez,
         `2258` = plugin_links_entrez,
         `3145` = plugin_links_copernicus,
         `246` = plugin_links_biorxiv,
         `127` = plugin_links_entrez,
         `301` = plugin_links_cogent,
         `1968` = plugin_links_entrez
  )
}
