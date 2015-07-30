#' Get full text.
#' 
#' @export
#' @importFrom rentrez entrez_search entrez_fetch
#' 
#' @param x Identifiers for papers, either DOIs, or other ids.
#' @param query Query terms
#' @param from Source to query
#' @param plosopts PLOS options. See \code{\link[rplos]{plos_fulltext}}
#' @param bmcopts BMC options. See \code{\link{bmc_xml}}
#' @param entrezopts Entrez options. See \code{\link[rentrez]{entrez_search}} and 
#' \code{\link{entrez_fetch}}
#' @param elifeopts eLife options
#' @param cache (logical) To cache results or not. If \code{cache=TRUE}, raw XML, or other
#' format that article is in is written to disk, then pulled from disk when further 
#' manipulations are done on the data. See also \code{\link{cache}}
#' @param backend (character) One of rds, rcache, or redis
#' @param path (character) Path to local folder
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' 
#' @return An object of class \code{ft_data}
#'
#' @examples \donttest{
#' # by default, plos gives back xml
#' ft_get('10.1371/journal.pone.0086169', from='plos')
#' 
#' # instead, get json
#' ft_get('10.1371/journal.pone.0086169', from='plos', plosopts=list(wt="json"))
#' 
#' (dois <- searchplos(q="*:*", fl='id', 
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' ft_get(dois, from='plos')
#' ft_get(c('10.7717/peerj.228','10.7717/peerj.234'), from='entrez')
#' 
#' # elife
#' ft_get('10.7554/eLife.04300', from='elife')
#' ft_get(c('10.7554/eLife.04300','10.7554/eLife.03032'), from='elife')
#' ##### replace with code not using elife packages
#' dois <- searchelife(terms="Cell biology", searchin="subject_area", boolean="contains")
#' ft_get(dois[1:10], from='elife')
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
#' res$entrez$data$DOI
#' ft_get(res$entrez$data$DOI[1], from='entrez')
#' ft_get(res$entrez$data$DOI[1:3], from='entrez')
#' 
#' # Caching
#' res <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")
#' 
#' # Search entrez, and pass to ft_get()
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res)
#' }
  
ft_get <- function(x, query, from='plos', plosopts=list(), bmcopts=list(), entrezopts=list(), elifeopts=list(), 
                   cache=FALSE, backend="rds", path="~/.fulltext", ...){
  UseMethod("ft_get")
}

#' @export
#' @rdname ft_get
ft_get.character <- function(x, query, from='plos', plosopts=list(), bmcopts=list(), entrezopts=list(), 
                   elifeopts=list(), cache=FALSE, backend="rds", path="~/.fulltext", ...){
  from <- match.arg(from, c("plos", "entrez", "bmc", "elife"))
  cacheopts <- cache_options_get()
  if (is.null(cacheopts$cache) && is.null(cacheopts$backend)) cache_options_set(cache, backend, path)
  
  plos_out <- plugin_get_plos(from, x, plosopts, ...)
  entrez_out <- plugin_get_entrez(from, x, entrezopts, ...)
  bmc_out <- plugin_get_bmc(from, x, bmcopts, ...)
  elife_out <- plugin_get_elife(from, x, elifeopts, ...)
  structure(list(plos = plos_out, entrez = entrez_out, bmc = bmc_out, elife = elife_out), class = "ft_data")
}

#' @export
#' @rdname ft_get
ft_get.ft <- function(x, query, from=NULL, plosopts=list(), bmcopts=list(), entrezopts=list(),  
                      elifeopts=list(), cache=FALSE, backend="rds", path="~/.fulltext", ...){
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
  alldois <- unlist(ft_compact(pluck(x, "dois")))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(unlist(pluck(x, "found")))
#   lengths <- 
#     unlist( sapply(x, function(y){ 
#       if(!is.null(y$data)) vapply(y$data, nchar, 1) else NULL 
#       # if(!is.null(y$data)) vapply(y$data, function(z) nchar(getChildrenStrings(z)), 1) else NULL 
#     }))
  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s %s", print_backend(cache_options_get()$backend), expand_if(cache_options_get()$path)), "\n")
  # cat(sprintf("[Size] Min. Length: %s - Max. Length: %s", min(lengths), max(lengths)), "\n")
  cat(ft_wrap(sprintf("[IDs]\n %s ...", namesprint)), "\n\n")
}

ft_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, width = width)
  paste0(wrapped, collapse = "\n")
}

expand_if <- function(x){
  if(!is.null(x)) paste0("- ", path.expand(x)) else ""
}

print_backend <- function(x){
  if(!is.null(x)) x else "R session"
}
