#' Search for full text.
#' 
#' @export
#' @import rplos bmc
#' 
#' @param query Query terms
#' @param from Source to query
#' @param limit Number of records to return.
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param ... Further args passed on to \code{httr::GET}
#' @param x Input to print method.
#' 
#' @return An object of class ft.
#'
#' @examples \dontrun{
#' ft_search(query='ecology', from='plos')
#' ft_search(query='climate change', from='plos', limit=500, plosopts=list(
#'    fl=c('id','author','eissn','journal','counter_total_all','alm_twitterCount'), fq='doc_type:full'))
#' }

ft_search <- function(query, from='plos', limit=10, plosopts=list(), bmcopts=list(), ...){
  plos_out <- plugin_plos(from, query, limit, plosopts)
  bmc_out <- plugin_bmc(from, query, limit, bmcopts)
  res <- list(plos=plos_out, bmc=bmc_out)
  class(res) <- "ft"
  res
}

#' Print brief summary of ft object
#'
#' @examples \dontrun{
#' xxx
#' }
#'
#' @param x Input...
#' @param ... Ignored.
#' @param n Number of data frame rows to print
#' @method print ft
#' @export

print.ft <- function(x, ..., n = 10) {
  rows <- sum(sapply(x, function(y) NROW(y$data)))
  found <- sum(unlist(ft_compact(sapply(x, "[[", 'found'))))
  
  cat(sprintf("Query [%s]", x[[1]]$opts$q), "\n")
  cat(sprintf("Records found, returned [%s,%s]", found, rows), "\n")
  cat(paste(sprintf("PLoS: %s", NROW(x$plos$data)), sprintf("BMC: %s", NROW(x$bmc$data)), sep = "; "), "\n")
  ft_trunc_mat(x$plos$data, n = n)
}

plugin_plos <- function(sources, query, limit, opts){
  if(any(grepl("plos", sources))){
    opts$q <- query
    opts$limit <- limit
    out <- do.call(searchplos, opts)
    list(found = out$meta$numFound, data = out$data, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}

plugin_bmc <- function(sources, query, limit, opts){
  if(any(grepl("bmc", sources))){
    opts$q <- query
    out <- do.call(bmc_search, opts)
    dat <- out
    list(found = out$meta$count, data = dat, opts = opts)
  } else {
    list(found = NULL, data = NULL, opts = opts)
  }
}
