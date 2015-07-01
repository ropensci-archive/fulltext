#' Search for full text.
#'
#' @export
#' @importFrom rplos searchplos plos_fulltext
#' @importFrom bmc bmc_search bmc_xml
#' @importFrom rcrossref cr_works
#' @importFrom plyr rbind.fill
#' @importFrom rentrez entrez_summary
#' @importFrom aRxiv arxiv_search
#' @importFrom biorxivr bx_search
#'
#' @param query Query terms
#' @param from Source to query
#' @param limit Number of records to return.
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param crossrefopts Crossref options. See \code{?cr_works}
#' @param entrezopts Entrez options. See \code{?entrez_search}
#' @param arxivopts arxiv options. See \code{?arxiv_search}
#' @param biorxivropts biorxivr options. See \code{?bx_search}
#' @param ... Further args passed on to \code{\link[httr]{GET}}. Not working right now...
#'
#' @return An object of class ft, and objects of class ft_ind within each source
#'
#' @examples \dontrun{
#' # Plos
#' (res <- ft_search(query='ecology', from='plos'))
#' res$plos
#' ft_search(query='climate change', from='plos', limit=500, plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all','alm_twitterCount')))
#'
#' # Crossref
#' (res <- ft_search(query='ecology', from='crossref'))
#' res$crossref
#'
#' #biorxivr
#' (res <- ft_search(query='ecology', from='biorxivr'))
#' res$crossref
#'
#' # BMC
#' (res <- ft_search(query='ecology', from='bmc'))
#' res$bmc
#'
#' # Entrez
#' (res <- ft_search(query='ecology', from='entrez'))
#' res$entrez
#'
#' # arxiv
#' (res <- ft_search(query='ecology', from='arxiv'))
#' res$arxiv
#'
#' # PLOS, Crossref, and arxiv
#' (res <- ft_search(query='ecology', from=c('plos','crossref','arxiv')))
#' res$plos
#' res$arxiv
#' res$crossref
#' }

ft_search <- function(query, from='plos', limit=10,
                      plosopts=list(),
                      bmcopts=list(),
                      crossrefopts=list(),
                      entrezopts=list(),
                      arxivopts=list(),
                      biorxivropts=list(),
                      ...) {
  
  plos_out <- plugin_plos(from, query, limit, plosopts)
  bmc_out <- plugin_bmc(from, query, limit, bmcopts)
  cr_out <- plugin_crossref(from, query, limit, crossrefopts)
  en_out <- plugin_entrez(from, query, limit, entrezopts)
  arx_out <- plugin_arxiv(from, query, limit, arxivopts)
  biorxivr_out <- plugin_biorxivr(from,query,limit,biorxivropts)

  res <- list(plos = plos_out, bmc = bmc_out, crossref = cr_out, 
              entrez = en_out, arxiv = arx_out, biorxivr = biorxivr_out)
  structure(res, class = "ft", query = query)
}

#' @export
print.ft <- function(x, ...) {
  rows <- sum(sapply(x, function(y) NROW(y$data)))
  found <- sum(unlist(ft_compact(sapply(x, "[[", 'found'))))

  cat(sprintf("Query:\n  [%s]", attr(x, "query")), "\n")

  cat("Found:\n")
  cat(paste(
    sprintf("  [PLoS: %s", null_len(x$plos$found)),
    sprintf("BMC: %s", null_len(x$bmc$found)),
    sprintf("Crossref: %s", null_len(x$crossref$found)),
    sprintf("Entrez: %s", null_len(x$entrez$found)),
    sprintf("arxiv: %s", null_len(x$arxiv$found)),
    sprintf("biorxiv: %s]", null_len(x$biorxivr$found)),
    sep = "; "), "\n")

  cat("Returned:\n")
  cat(paste(
    sprintf("  [PLoS: %s", NROW(x$plos$data)),
    sprintf("BMC: %s", NROW(x$bmc$data)),
    sprintf("Crossref: %s", NROW(x$crossref$data)),
    sprintf("Entrez: %s", NROW(x$entrez$data)),
    sprintf("arxiv: %s", NROW(x$arxiv$data)),
    sprintf("biorxiv: %s]", NROW(x$biorxivr$data)),
    sep = "; "), "\n")
}

#' @export
print.ft_ind <- function(x, ..., n = 10) {
  rows <- NROW(x$data)
  found <- x$found
  cat(sprintf("Query: [%s]", attr(x, "query")), "\n")
  cat(sprintf("Records found, returned: [%s, %s]", found, rows), "\n")
  cat(sprintf("License: [all %s]", x$license$type), "\n")
  print_if(x$data, n = n)
}

null_len <- function(x) if (is.null(x)) 0 else x

print_if <- function(x, n) {
  if (!is.null(x)) ft_trunc_mat(x, n)
}
