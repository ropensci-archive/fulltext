#' Get full text.
#' 
#' @export
#' @importFrom elife elife_paper
#' @importFrom rentrez entrez_search entrez_fetch
#' 
#' @param ids Identifiers for papers, either DOIs, or other ids.
#' @param query Query terms
#' @param from Source to query
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param entrezopts Entrez options. See \code{?entrez_search} and \code{?entrez_fetch}
#' @param elifeopts eLife options. See \code{?elife_search}
#' @param ... Further args passed on to \code{httr::GET}
#' 
#' @return An object of class ft_data
#'
#' @examples \donttest{
#' ft_get(ids='10.1371/journal.pone.0086169', from='plos')
#' (dois <- searchplos(q="*:*", fl='id', fq='doc_type:full', limit=5)$data$id)
#' ft_get(ids=dois, from='plos')
#' ft_get(ids=c('10.7717/peerj.228','10.7717/peerj.234'), from='entrez')
#' 
#' # elife
#' ft_get(ids=c('10.7554/eLife.04300','10.7554/eLife.03032'), from='elife')
#' library('elife')
#' dois <- searchelife(terms="Cell biology", searchin="subject_area", boolean="contains")
#' ft_get(ids=dois[1:10], from='elife')
#' 
#' # bmc
#' ft_get(ids='http://www.microbiomejournal.com/content/download/xml/2049-2618-2-7.xml', from='bmc')
#' 
#' # Frontiers in Pharmacology (publisher: Frontiers)
#' doi <- '10.3389/fphar.2014.00109'
#' ft_get(ids=doi, from="entrez")
#' 
#' # Hindawi Journals
#' ft_get(ids=c('10.1155/2014/292109','10.1155/2014/162024','10.1155/2014/249309'), from='entrez')
#' res <- ft_search(query='ecology', from='crossref', limit=50,
#'                  crossrefopts = list(filter=list(has_full_text = TRUE, 
#'                                                  member=98, 
#'                                                  type='journal-article')))
#' 
#' out <- ft_get(ids=res$crossref$data$DOI[1:20], from='entrez')
#' }

ft_get <- function(ids, query, from='plos', plosopts=list(), bmcopts=list(), entrezopts=list(), 
  elifeopts=list(), ...)
{
  plos_out <- plugin_get_plos(from, ids, plosopts, ...)
  entrez_out <- plugin_get_entrez(from, ids, entrezopts, ...)
  bmc_out <- plugin_get_bmc(from, ids, bmcopts, ...)
  elife_out <- plugin_get_elife(from, ids, elifeopts, ...)
  res <- list(plos=plos_out, entrez=entrez_out, bmc=bmc_out, elife=elife_out)
  class(res) <- "ft_data"
  res
}

#' Print brief summary of ft_data object
#'
#' @param x Input...
#' @param ... Ignored.
#' @method print ft_data
#' @export

print.ft_data <- function(x, ...) {
  alldois <- unlist(ft_compact(sapply(x, function(z) names(z$data))))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(sapply(x, function(y) length(y$data)))
  lengths <- unlist( sapply(x, function(y){ if(!is.null(y$data)) vapply(y$data, nchar, 1) else NULL }) )
  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s", "R session"), "\n")
  cat(sprintf("[Size] Min. Length: %s - Max. Length: %s", min(lengths), max(lengths)), "\n")
  cat(ft_wrap(sprintf("[IDs]\n %s ...", namesprint)), "\n\n")
}

plugin_get_plos <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("plos", sources))){
    opts$doi <- ids
    opts$callopts <- callopts
    out <- do.call(plos_fulltext, opts)
    attr(out, "format") <- "xml"
    list(found = length(out), data = out, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}

plugin_get_entrez <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("entrez", sources))){
    opts$ids <- ids
    out <- do.call(entrez_get, opts)
    attr(out, "format") <- "xml"
    list(found = length(out), data = out, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}

entrez_get <- function(ids){
  res <- entrez_search(db="pmc", term=paste0(sprintf('%s[doi]', ids), collapse = "|"))
  vapply(res$ids, function(z) entrez_fetch(db = 'pmc', id=z, rettype = "xml"), character(1))
}

plugin_get_bmc <- function(sources, query, opts, ...){
  callopts <- list(...)
  if(any(grepl("bmc", sources))){
    opts$uris <- query
    opts$raw <- TRUE
    out <- do.call(bmc_xml, opts)
    attr(out, "format") <- "xml"
    list(found = length(out), data = out, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}

plugin_get_elife <- function(sources, ids, opts, ...){
  callopts <- list(...)
  if(any(grepl("elife", sources))){
    opts$doi <- ids
    opts$what <- "text"
    out2 <- lapply(ids, elife_paper, what="text")
    names(out2) <- ids
    attr(out2, "format") <- "xml"
    list(found = length(out2), data = out2, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}

ft_wrap <- function (..., indent = 0, width = getOption("width")){
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, width = width)
  paste0(wrapped, collapse = "\n")
}
