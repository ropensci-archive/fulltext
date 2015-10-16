#' @title Get full text
#'
#' @description \code{ft_get} is a one stop shop to fetch full text of articles,
#' either XML or PDFs. We have specific support for PLOS via the
#' \code{rplos} package, Entrez via the \code{rentrez} package, and arXiv via the
#' \code{aRxiv} package. For other publishers, we have helpers to \code{ft_get} to
#' sort out links for full text based on user input. See \code{Details} for
#' help on how to use this function.
#'
#' @export
#'
#' @param x Either identifiers for papers, either DOIs (or other ids) as a list of
#' charcter strings, or a character vector, OR an object of class \code{ft}, as
#' returned from \code{\link{ft_search}}
#' @param from Source to query. Optional.
#' @param plosopts PLOS options. See \code{\link[rplos]{plos_fulltext}}
#' @param bmcopts BMC options. See \code{\link{bmc_xml}}
#' @param entrezopts Entrez options. See \code{\link[rentrez]{entrez_search}} and
#' \code{\link{entrez_fetch}}
#' @param elifeopts eLife options
#' @param cache (logical) To cache results or not. If \code{cache=TRUE}, raw XML, or other
#' format that article is in is written to disk, then pulled from disk when further
#' manipulations are done on the data. See also \code{\link{cache}}
#' @param backend (character) One of rds, rcache, or redis
#' @param path (character) Path to local folder. If the folder doesn't exist, we
#' create it for you.
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#'
#' @return An object of class \code{ft_data} (of type \code{S3}) with slots for
#' each of the publishers. The returned object is split up by publishers because
#' the full text format is the same within publisher - which should facilitate
#' text mining downstream as different steps may be needed for each publisher's
#' content.
#'
#' @details There are various ways to use \code{ft_get}:
#' \itemize{
#'  \item Pass in only DOIs - leave \code{from} parameter \code{NULL}. This route will
#'  first query Crossref API for the publisher of the DOI, then we'll use the appropriate
#'  method to fetch full text from the publisher. If a publisher is not found for the DOI,
#'  then we'll throw back a message telling you a publisher was not found.
#'  \item Pass in DOIs (or other pub IDs) and use the \code{from} parameter. This route
#'  means we don't have to make an extra API call to Crossref (thus, this route is faster)
#'  to determine the publisher for each DOI. We go straight to getting full text based on
#'  the publisher.
#'  \item Use \code{\link{ft_search}} to search for articles. Then pass that output to
#'  this function, which will use info in that object. This behaves the same as the previous
#'  option in that each DOI has publisher info so we know how to get full text for each
#'  DOI.
#' }
#'
#' Note that some publishers are available via Entrez, but often not recent articles,
#' where "recent" may be a few months to a year or so. In that case, make sure to specify
#' the publisher, or else you'll get back no data.
#'
#' @section Notes on specific publishers:
#' \itemize{
#'  \item arXiv - The IDs passed are not actually DOIs, though they look similar.
#'  Thus, there's no way to not pass in the \code{from} parameter as we can't
#'  determine unambiguously that the IDs passed in are from arXiv.org.
#' }
#'
#' @examples \dontrun{
#' # If you just have DOIs and don't know the publisher
#' ## PLOS
#' ft_get('10.1371/journal.pone.0086169')
#' ## PeerJ
#' ft_get('10.7717/peerj.228')
#' ## eLife
#' ft_get('10.7554/eLife.03032')
#' ## BMC
#' ft_get(c('10.1186/2049-2618-2-7', '10.1186/2193-1801-3-7'))
#' ## FrontiersIn
#' res <- ft_get(c('10.3389/fphar.2014.00109', '10.3389/feart.2015.00009'))
#' ## Hindawi - via Entrez
#' res <- ft_get(c('10.1155/2014/292109','10.1155/2014/162024','10.1155/2014/249309'))
#' ## F1000Research - via Entrez
#' ft_get('10.12688/f1000research.6522.1')
#' ## Two different publishers via Entrez - retains publisher names
#' res <- ft_get(c('10.1155/2014/292109', '10.12688/f1000research.6522.1'))
#' res$hindawi
#' res$f1000research
#' ## Pensoft
#' ft_get('10.3897/zookeys.499.8360')
#' ### you'll need to specify the publisher for a DOI from a recent publication
#' ft_get('10.3897/zookeys.515.9332', from = "pensoft")
#' ## Copernicus
#' out <- ft_get(c('10.5194/angeo-31-2157-2013', '10.5194/bg-12-4577-2015'))
#' out$copernicus
#' ## arXiv - only pdf, you have to pass in the from parameter
#' res <- ft_get(x='cond-mat/9309029', from = "arxiv", cache=TRUE, backend="rds")
#' res %>% ft_extract
#' ## bioRxiv - only pdf
#' res <- ft_get(x='10.1101/012476')
#' res$biorxiv
#' ## Karger Publisher
#' ft_get('10.1159/000369331')
#' ## CogentOA Publisher
#' ft_get('10.1080/23311916.2014.938430')
#' ## MDPI Publisher
#' ft_get('10.3390/nu3010063')
#' ft_get('10.3390/nu7085279')
#' ft_get(c('10.3390/nu3010063', '10.3390/nu7085279')) # not working, only getting 1
#'
#' # If you know the publisher, give DOI and publisher
#' ## by default, PLOS gives back XML
#' ft_get('10.1371/journal.pone.0086169', from='plos')
#' ## you can instead get json
#' ft_get('10.1371/journal.pone.0086169', from='plos', plosopts=list(wt="json"))
#'
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' ft_get(dois, from='plos')
#' ft_get(c('10.7717/peerj.228','10.7717/peerj.234'), from='entrez')
#'
#' # elife
#' ft_get('10.7554/eLife.04300', from='elife')
#' ft_get(c('10.7554/eLife.04300', '10.7554/eLife.03032'), from='elife')
#' ## search for elife papers via Entrez
#' dois <- ft_search("elife[journal]", from = "entrez")
#' ft_get(dois)
#'
#' # bmc
#' ft_get('http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml', from='bmc')
#' urls <- c('http://www.biomedcentral.com/content/download/xml/1471-2393-14-71.xml',
#'  'http://www.springerplus.com/content/download/xml/2193-1801-3-7.xml',
#'  'http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml')
#' ft_get(urls, from='bmc')
#'
#' # Frontiers in Pharmacology (publisher: Frontiers)
#' doi <- '10.3389/fphar.2014.00109'
#' ft_get(doi, from="entrez")
#'
#' # Hindawi Journals
#' ft_get(c('10.1155/2014/292109','10.1155/2014/162024','10.1155/2014/249309'), from='entrez')
#' res <- ft_search(query='ecology', from='crossref', limit=50,
#'                  crossrefopts = list(filter=list(has_full_text = TRUE,
#'                                                  member=98,
#'                                                  type='journal-article')))
#'
#' out <- ft_get(res$crossref$data$DOI[1:20], from='entrez')
#'
#' # Frontiers Publisher - Frontiers in Aging Nueroscience
#' res <- ft_get("10.3389/fnagi.2014.00130", from='entrez')
#' res$entrez
#'
#' # Search entrez, get some DOIs
#' (res <- ft_search(query='ecology', from='entrez'))
#' res$entrez$data$doi
#' ft_get(res$entrez$data$doi[1], from='entrez')
#' ft_get(res$entrez$data$doi[1:3], from='entrez')
#'
#' # Caching
#' res <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")
#'
#' # Search entrez, and pass to ft_get()
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res)
#' }

ft_get <- function(x, from=NULL, plosopts=list(), bmcopts=list(), entrezopts=list(), elifeopts=list(),
                   cache=FALSE, backend="rds", path="~/.fulltext", ...){
  UseMethod("ft_get")
}

#' @export
#' @rdname ft_get
ft_get.character <- function(x, from=NULL, plosopts=list(), bmcopts=list(), entrezopts=list(),
                   elifeopts=list(), cache=FALSE, backend="rds", path="~/.fulltext", ...) {

  make_dir(path)
  cacheopts <- cache_options_get()
  if (is.null(cacheopts$cache) && is.null(cacheopts$backend)) cache_options_set(cache, backend, path)

  if (!is.null(from)) {
    from <- match.arg(from, c("plos", "entrez", "bmc", "elife", "pensoft", "arxiv", "biorxiv"))
    plos_out <- plugin_get_plos(from, x, plosopts, ...)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, ...)
    bmc_out <- plugin_get_bmc(from, x, bmcopts, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, ...)
    pensoft_out <- plugin_get_pensoft(from, x, list(), ...)
    arxiv_out <- plugin_get_arxiv(from, x, list(), path, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, list(), path, ...)
    structure(list(plos = plos_out, entrez = entrez_out, bmc = bmc_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out, biorxiv = biorxiv_out), class = "ft_data")
  } else {
    get_unknown(x, path, ...)
  }
}

#' @export
#' @rdname ft_get
ft_get.list <- function(x, from=NULL, plosopts=list(), bmcopts=list(), entrezopts=list(),
                        elifeopts=list(), cache=FALSE, backend="rds", path="~/.fulltext", ...) {

  make_dir(path)
  cacheopts <- cache_options_get()
  if (is.null(cacheopts$cache) && is.null(cacheopts$backend)) cache_options_set(cache, backend, path)

  if (!is.null(from)) {
    from <- match.arg(from, c("plos", "entrez", "bmc", "elife", "pensoft", "arxiv", "biorxiv"))
    plos_out <- plugin_get_plos(from, x, plosopts, ...)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, ...)
    bmc_out <- plugin_get_bmc(from, x, bmcopts, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, ...)
    pensoft_out <- plugin_get_pensoft(from, x, ...)
    arxiv_out <- plugin_get_arxiv(from, x, path, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, path, ...)
    structure(list(plos = plos_out, entrez = entrez_out, bmc = bmc_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out, biorxiv = biorxiv_out), class = "ft_data")
  } else {
    get_unknown(x, path, ...)
  }
}

#' @export
#' @rdname ft_get
ft_get.ft <- function(x, from=NULL, plosopts=list(), bmcopts=list(), entrezopts=list(),
                      elifeopts=list(), cache=FALSE, backend="rds", path="~/.fulltext", ...) {

  make_dir(path)
  cacheopts <- cache_options_get()
  if (is.null(cacheopts$cache) && is.null(cacheopts$backend)) cache_options_set(cache, backend, path)

  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  plos_out <- plugin_get_plos(from, x$plos$data$id, plosopts, ...)
  entrez_out <- plugin_get_entrez(from, x$entrez$data$doi, entrezopts, ...)
  bmc_out <- plugin_get_bmc(from, x$bmc$data, bmcopts, ...)
  structure(list(plos = plos_out, entrez = entrez_out, bmc = bmc_out), class = "ft_data")
}

#' @export
print.ft_data <- function(x, ...) {
  cat("<fulltext text>", sep = "\n")
  alldois <- unlist(ft_compact(pluck(x, "dois")))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(unlist(pluck(x, "found")))
  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s %s", print_backend(cache_options_get()$backend), expand_if(cache_options_get()$path)), "\n")
  cat(ft_wrap(sprintf("[IDs]\n %s ...", namesprint)), "\n\n")
}

ft_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, width = width)
  paste0(wrapped, collapse = "\n")
}

expand_if <- function(x) {
  if (!is.null(x)) paste0("- ", path.expand(x)) else ""
}

print_backend <- function(x) {
  if (!is.null(x)) x else "R session"
}

make_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

# get unknown from DOIs where from=NULL ------------------
get_unknown <- function(x, path, ...) {
  pubs <- ft_compact(sapply(x, get_publisher))
  df <- data.frame(pub = unlist(unname(pubs)), doi = names(pubs), stringsAsFactors = FALSE)
  dfsplit <- split(df, df$pub)
  out <- list()
  for (i in seq_along(dfsplit)) {
    fun <- publisher_plugin(names(dfsplit)[i])
    pub_nm <- get_pub_name(names(dfsplit)[i])
    tm_nm <- get_tm_name(names(dfsplit)[i])
    out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), path = path, ...)
  }
  structure(out, class = "ft_data")
}

publisher_plugin <- function(x) {
  switch(x,
         `4374` = plugin_get_elife,
         `340` = plugin_get_plos,
         `4443` = plugin_get_peerj,
         `297` = plugin_get_bmc,
         `1965` = plugin_get_frontiersin,
         `98` = plugin_get_entrez,
         `4950` = plugin_get_entrez,
         `2258` = plugin_get_entrez,
         `3145` = plugin_get_copernicus,
         `246` = plugin_get_biorxiv,
         `127` = plugin_get_entrez,
         `301` = plugin_get_cogent,
         `1968` = plugin_get_entrez
  )
}

get_pub_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         `297` = "bmc",
         `1965` = "frontiersin",
         `98` = "hindawi",
         `4950` = "f1000research",
         `2258` = "pensoft",
         `3145` = "copernicus",
         `246` = "biorxiv",
         `127` = "karger",
         `301` = "cogent",
         `1968` = "mdpi"
  )
}

get_tm_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         `297` = "bmc",
         `1965` = "frontiersin",
         `98` = "entrez",
         `4950` = "entrez",
         `2258` = "entrez",
         `3145` = "copernicus",
         `246` = "biorxiv",
         `127` = "entrez",
         `301` = "cogent",
         `1968` = "entrez"
  )
}

get_publisher <- function(x) {
  z <- tryCatch(cr_works(x)$data$member, warning = function(e) e)
  if (is(z, "warning")) {
    NULL
  } else {
    as.character(strextract(z, "[0-9]+"))
  }
}
