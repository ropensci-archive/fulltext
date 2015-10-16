#' Get full text links
#'
#' @export
#' @param x One of \code{ft}, \code{ft_ind}, or a character string of DOIs.
#' @param from Source to query. Ignored when \code{ft_ind} class passed.
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param crossrefopts Crossref options. See \code{?cr_works}
#' @param entrezopts Entrez options. See \code{?entrez_search}
#' @param ... Further args passed on to \code{\link[httr]{GET}}. Not working right now...
#'
#' @return An object of class ft_links, with either a list or data.frame for each 
#' DOI, with links for XML and PDF links (typically). 
#' 
#' @details Inputs can be an object of class \code{ft}, \code{ft_ind}, or a 
#' character string of DOIs. You can specify a specific source for four sources
#' (PLOS, BMC, Crossref, and Entrez), but any other publishers we guess the 
#' publisher form the input DOI(s), then attempt to generate full text links 
#' based on the publisher (if found). Of course, guessing the publisher makes
#' things slower as it requires an HTTP request.
#' 
#' Strategy varies by publisher. For some we can construct XML and PDF links 
#' only from the DOI. For others, we need to make an HTTP request to the 
#' publisher to get additional information - this of course makes things slower.
#'
#' @examples \dontrun{
#' # Entrez
#' (res1 <- ft_search(query='ecology', from='entrez'))
#' res1$entrez$data$doi
#' ## directly from ft_search output
#' (out <- ft_links(res1))
#' out$entrez
#' out$entrez$data[[1]]
#' ## directly individual elements of ft_search output
#' (out <- ft_links(res1$entrez))
#' out$entrez
#' ## from character vector of DOIs
#' x <- c("10.1371/journal.pone.0086169", "10.1016/j.ympev.2010.07.013")
#' (out2 <- ft_links(x, from = "entrez"))
#' out2$entrez
#' 
#' # Crossref
#' (res2 <- ft_search(query='ecology', from='crossref'))
#' res2$crossref$data$doi
#' ## directly from ft_search output
#' (out <- ft_links(res2))
#' out$crossref
#' out$crossref$data[[1]]
#' ## directly individual elements of ft_search output
#' (out <- ft_links(res2$crossref))
#' out$crossref
#' ## from character vector of DOIs
#' x <- c("10.1016/s1754-5048(14)00139-1", "10.1016/b978-0-12-378260-1.50017-8")
#' (out2 <- ft_links(x, from = "crossref"))
#' out2$crossref
#' 
#' # PLOS
#' (res3 <- ft_search(query='ecology', from='plos', plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all','alm_twitterCount'))))
#' res3$plos$data$id
#' ## directly from ft_search output
#' (out <- ft_links(res3))
#' out$plos
#' out$plos$data[[1]]
#' ## directly individual elements of ft_search output
#' (out <- ft_links(res3$plos))
#' out$plos
#' ## from character vector of DOIs
#' x <- c("10.1371/journal.pone.0017342", "10.1371/journal.pone.0091497")
#' out3 <- ft_links(x, from = "plos")
#' out3$plos
#'
#' # BMC
#' (res <- ft_search(query='ecology', from='bmc'))
#' res$bmc
#' ## directly from ft_search output
#' (out <- ft_links(res))
#' out$bmc
#' out$bmc$data[[1]]
#' ## directly individual elements of ft_search output
#' (out <- ft_links(res$bmc))
#' out$bmc
#' 
#' # Character input
#' out4 <- ft_links('10.1371/journal.pone.0086169')
#' out4$plos
#' 
#' # other publishers
#' ## elife
#' res <- ft_links(c('10.7554/eLife.03032', '10.7554/eLife.02747'))
#' res$elife
#' 
#' ## peerj
#' ft_links('10.7717/peerj.228')
#' ft_links(c('10.7717/peerj.228', '10.7717/peerj.1200'))
#' 
#' ## frontiersin
#' (res <- ft_links('10.3389/fphar.2014.00109'))
#' res$frontiersin
#' 
#' ## copernicus
#' (res <- ft_links('10.5194/angeo-31-2157-2013'))
#' res$copernicus
#' 
#' ## cogent
#' (res <- ft_links('10.1080/23311916.2014.938430'))
#' res$cogent
#' 
#' ## bmc
#' (res <- ft_links('10.1186/2049-2618-2-7'))
#' res$bmc
#' (res <- ft_links('10.1186/2049-2618-2-7', from = "bmc"))
#' 
#' ## Many publishers, elife and peerj
#' res <- ft_links(c('10.7554/eLife.03032', '10.7717/peerj.228'))
#' res$elife
#' res$peerj
#' }
ft_links <- function(x, from = NULL, plosopts = list(), crossrefopts = list(),
                    entrezopts = list(), bmcopts = list(), ...) {
  UseMethod("ft_links")
}

#' @export
#' @rdname ft_links
ft_links.ft <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  plos_out <- plugin_links_plos(from, x$plos$data$id, plosopts)
  bmc_out <- plugin_links_bmc(from, x$bmc$data$doi, bmcopts)
  cr_out <- plugin_links_crossref(from, x$crossref$data$doi, crossrefopts)
  en_out <- plugin_links_entrez(from, x$entrez$data$doi, entrezopts)
  res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
  structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
}

#' @export
#' @rdname ft_links
ft_links.ft_ind <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  from <- x$source
  plos_out <- plugin_links_plos(from, x$data$id, plosopts)
  bmc_out <- plugin_links_bmc(from, x$data$doi, bmcopts)
  cr_out <- plugin_links_crossref(from, x$data$doi, crossrefopts)
  en_out <- plugin_links_entrez(from, x$data$doi, entrezopts)
  res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
  structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
}

#' @export
#' @rdname ft_links
ft_links.character <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  if (!is.null(from)) {
    plos_out <- plugin_links_plos(from, x, plosopts)
    bmc_out <- plugin_links_bmc(from, x, bmcopts)
    cr_out <- plugin_links_crossref(from, x, crossrefopts)
    en_out <- plugin_links_entrez(from, x, entrezopts)
    # arx_out <- plugin_links_arxiv(from, x, arxivopts)
    # bio_out <- plugin_links_biorxiv(from, x, biorxivopts)
    
    res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
    structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
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
         `1965` = plugin_links_frontiersin,
         `98` = plugin_links_entrez,
         `4950` = plugin_links_entrez,
         `2258` = plugin_links_entrez,
         `3145` = plugin_links_copernicus,
         # `246` = plugin_links_biorxiv,
         `127` = plugin_links_entrez,
         `301` = plugin_links_cogent,
         `1968` = plugin_links_entrez,
         `297` = plugin_links_bmc
  )
}
