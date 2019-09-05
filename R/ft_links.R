#' Get full text links
#'
#' @export
#' @param x One of `ft`, `ft_ind`, or a character string of DOIs.
#' @param from Source to query. Ignored when `ft_ind` class passed.
#' @param plosopts PLOS options, a named list. See `?searchplos`
#' @param bmcopts BMC options, a named list. See `?bmc_search`
#' @param crossrefopts Crossref options, a named list. See `?cr_works`
#' @param entrezopts Entrez options, a named list. See `?entrez_search`
#' @param ... curl options passed on to [crul::HttpClient] (plos, bmc,
#' crossref) or `httr::GET()` (entrez), see examples below
#'
#' @return An object of class ft_links, with either a list or data.frame for 
#' each DOI, with links for XML and PDF links (typically). 
#' 
#' @details Inputs can be an object of class `ft`, `ft_ind`, or a 
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
#' See **Rate Limits** and **Authentication** in 
#' [fulltext-package] for rate limiting and authentication information,
#' respectively
#' 
#' @examples 
#' # List publishers included
#' ft_links_ls()
#' 
#' \dontrun{
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
#' x <- c("10.1016/s1754-5048(14)00139-1", 
#'        "10.1016/b978-0-12-378260-1.50017-8")
#' (out2 <- ft_links(x, from = "crossref"))
#' out2$crossref
#' 
#' # PLOS
#' (res3 <- ft_search(query='ecology', from='plos', plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all',
#'         'alm_twitterCount'))))
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
#' ## wiley
#' res <- ft_links('10.1006/asle.2001.0035', from = "crossref")
#' res$crossref$data[[1]]$url
#' 
#' ## informa
#' res <- ft_links('10.1174/02134749660569378', from = "crossref")
#' res$crossref$data[[1]]$url
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
#' 
#' 
#' # curl options
#' ft_links("10.2458/v17i1.21696", from = "crossref", verbose = TRUE)
#' }
ft_links <- function(x, from = NULL, plosopts = list(), crossrefopts = list(),
                    entrezopts = list(), bmcopts = list(), ...) {
  UseMethod("ft_links")
}

#' @export
ft_links.ft <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  assert_from(from, c("plos", "bmc", "crossref", "entrez"))
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  plos_out <- plugin_links_plos(from, x$plos$data$id, plosopts, ...)
  bmc_out <- plugin_links_bmc(from, x$bmc$data$doi, bmcopts, ...)
  cr_out <- plugin_links_crossref(from, x$crossref$data$doi, crossrefopts, ...)
  en_out <- plugin_links_entrez(from, x$entrez$data$doi, entrezopts, ...)
  res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
  structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
}

#' @export
ft_links.ft_ind <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  assert_from(from, c("plos", "bmc", "crossref", "entrez"))
  from <- x$source
  plos_out <- plugin_links_plos(from, x$data$id, plosopts, ...)
  bmc_out <- plugin_links_bmc(from, x$data$doi, bmcopts, ...)
  cr_out <- plugin_links_crossref(from, x$data$doi, crossrefopts, ...)
  en_out <- plugin_links_entrez(from, x$data$doi, entrezopts, ...)
  res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
  structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
}

#' @export
ft_links.character <- function(x, from = NULL, 
                        plosopts = list(),
                        crossrefopts = list(),
                        entrezopts = list(),
                        bmcopts = list(),
                        ...) {
  
  assert_from(from, c("plos", "bmc", "crossref", "entrez"))
  if (!is.null(from)) {
    plos_out <- plugin_links_plos(from, x, plosopts, ...)
    bmc_out <- plugin_links_bmc(from, x, bmcopts, ...)
    cr_out <- plugin_links_crossref(from, x, crossrefopts, ...)
    en_out <- plugin_links_entrez(from, x, entrezopts, ...)
    # arx_out <- plugin_links_arxiv(from, x, arxivopts)
    # bio_out <- plugin_links_biorxiv(from, x, biorxivopts)
    
    res <- list(plos = plos_out, crossref = cr_out, entrez = en_out, bmc = bmc_out)
    structure(Filter(function(x) !is.null(x$data), res), class = "ft_links")
  } else {
    get_unknown_links(x, ...)
  }
}

#' @export
#' @rdname ft_links
ft_links_ls <- function() {
  nms <- ls(getNamespace("fulltext"), all.names = TRUE, pattern = "plugin_links_")
  gsub("plugin_links_", "", nms)
}


#' @export
print.ft_links <- function(x, ...) {
  cat("<fulltext links>", sep = "\n")
  alldois <- unlist(ft_compact(pluck(x, "ids")))
  namesprint <- paste(stats::na.omit(alldois[1:10]), collapse = " ")
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
    tm_nm <- get_tm_name_links(names(dfsplit)[i])
    if (inherits(fun, "function")) {
      out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), ...)
    } else {
      out[[ pub_nm ]] <- emptylist()
    }
  }
  structure(out, class = "ft_links")
}

publisher_plugin_links <- function(x) {
  switch(x, 
    `4374` = plugin_links_elife,
    `340` = plugin_links_plos,
    `4443` = plugin_links_peerj,
    `1965` = plugin_links_frontiersin,
    `98` = plugin_links_crossref,
    `4950` = plugin_links_entrez,
    `2258` = plugin_links_entrez,
    `3145` = plugin_links_copernicus,
    `127` = plugin_links_entrez,
    `301` = plugin_links_cogent,
    `1968` = plugin_links_entrez,
    # `297` = plugin_links_bmc,
    `297` = plugin_links_crossref,
    `179` = plugin_links_crossref,
    `311` = plugin_links_crossref,
    `78` = plugin_links_crossref,
    `2997` = plugin_links_crossref,
    `175` = plugin_links_rsoc,
    `1822` = plugin_links_cdc,
    warning("Crossref member ", x,
      " not supported yet; open an issue https://github.com/ropensci/fulltext/issues")
  )
}

get_tm_name_links <- function(x) {
  switch(x,
    `4374` = "elife",
    `340` = "plos",
    `4443` = "peerj",
    `1965` = "frontiersin",
    `98` = "crossref",
    `4950` = "entrez",
    `2258` = "pensoft",
    `3145` = "copernicus",
    `246` = "biorxiv",
    `127` = "entrez",
    `1968` = "entrez",
    `78` = "crossref",
    `311` = "crossref",
    `1665` = "scientificsocieties",
    `301` = "informa",
    `292` = "royalsocchem",
    `263` = "ieee",
    `221` = "aaas",
    `341` = "pnas",
    `345` = "microbiology",
    `10` = "jama",
    `235` = "amersocmicrobiol",
    `233` = "amersocclinoncol",
    `8215` = "instinvestfil",
    `317` = "aip",
    `297` = "crossref",
    `2997` = "crossref",
    `175` = "rsoc",
    `1822` = "cdc",
    "crossref"
  )
}
