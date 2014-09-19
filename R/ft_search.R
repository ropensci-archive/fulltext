#' Search for full text.
#' 
#' @export
#' @importFrom rplos searchplos plos_fulltext
#' @importFrom bmc bmc_search bmc_xml
#' @importFrom rcrossref cr_works
#' 
#' @param query Query terms
#' @param from Source to query
#' @param limit Number of records to return.
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param crossrefopts Crossref options. See \code{?cr_works}
#' @param ... Further args passed on to \code{httr::GET}
#' 
#' @return An object of class ft.
#'
#' @examples \dontrun{
#' # Plos
#' ft_search(query='ecology', from='plos')
#' ft_search(query='climate change', from='plos', limit=500, plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all','alm_twitterCount')))
#'    
#' # Crossref
#' ft_search(query='ecology', from='crossref')
#' 
#' # BMC
#' ft_search(query='ecology', from='bmc')
#' 
#' # Plos and Crossref
#' (res <- ft_search(query='ecology', from=c('plos','crossref')))
#' res$plos
#' res$crossref
#' }

ft_search <- function(query, from='plos', limit=10, plosopts=list(), bmcopts=list(), 
  crossrefopts=list(), ...)
{
  plos_out <- plugin_plos(from, query, limit, plosopts)
  bmc_out <- plugin_bmc(from, query, limit, bmcopts)
  cr_out <- plugin_crossref(from, query, limit, crossrefopts)
  res <- list(plos=plos_out, bmc=bmc_out, crossref=cr_out)
  structure(res, class="ft", query=query)
}

#' Print brief summary of ft object
#'
#' @param x Input...
#' @param ... Ignored.
#' @method print ft
#' @export

print.ft <- function(x, ...) {
  rows <- sum(sapply(x, function(y) NROW(y$data)))
  found <- sum(unlist(ft_compact(sapply(x, "[[", 'found'))))
  cat(sprintf("Query:\n  [%s]", attr(x, "query")), "\n")
  cat("Found:\n")
  cat(paste(
    sprintf("  [PLoS: %s", x$plos$found), 
    sprintf("BMC: %s", x$bmc$found), 
    sprintf("Crossref: %s]", x$crossref$found),
    sep = "; "), "\n")
  cat("Returned:\n")
  cat(paste(
    sprintf("  [PLoS: %s", NROW(x$plos$data)), 
    sprintf("BMC: %s", NROW(x$bmc$data)), 
    sprintf("Crossref: %s]", NROW(x$crossref$data)),
    sep = "; "), "\n")
}

#' Print brief summary of ftind object
#'
#' @param x Input...
#' @param ... Ignored.
#' @param n Number of data frame rows to print
#' @method print ftind
#' @export

print.ftind <- function(x, ..., n = 10) {
  rows <- NROW(x$data)
  found <- x$found
  cat(sprintf("Query: [%s]", x$opts$q), "\n")
  cat(sprintf("Records found, returned [%s, %s]", found, rows), "\n")
  print_if(x$data, n = n)
}

print_if <- function(x, n){
  if(!is.null(x)) ft_trunc_mat(x, n)
}

plugin_plos <- function(sources, query, limit, opts){
  if(any(grepl("plos", sources))){
    opts$q <- query
    opts$limit <- limit
    out <- do.call(searchplos, opts)
    zz <- list(found = out$meta$numFound, data = out$data, opts = opts)
    structure(zz, class="ftind")
  } else {
    zz <- list(found = NA, data = NA, opts = opts)
    structure(zz, class="ftind")
  }
}

plugin_crossref <- function(sources, query, limit, opts){
  if(any(grepl("crossref", sources))){
    opts$query <- query
    opts$filter <- c(has_full_text = TRUE)
    out <- do.call(cr_works, opts)
    zz <- list(found = out$meta$`total-results`, data = out$data, opts = opts)
    structure(zz, class="ftind")
  } else {
    zz <- list(found = NA, data = NA, opts = opts)
    structure(zz, class="ftind")
  }
}

plugin_bmc <- function(sources, query, limit, opts){
  if(any(grepl("bmc", sources))){
    opts$terms <- query
    out <- do.call(bmc_search, opts)
    dat <- do.call(rbind, lapply(out@results$entries, data.frame, stringsAsFactors = FALSE))
    opts$query <- opts$terms; opts$terms <- NULL
    zz <- list(found = NA, data = dat, opts = opts)
    structure(zz, class="ftind")
  } else {
    zz <- list(found = NA, data = NA, opts = opts)
    structure(zz, class="ftind")
  }
}
