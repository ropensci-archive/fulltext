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
#' }

ft_search <- function(query, from='plos', limit=10, plosopts=list(), bmcopts=list(), ...){
  plos_out <- plugin_plos(from, query, limit, plosopts)
  bmc_out <- plugin_bmc(from, query, limit, bmcopts)
  res <- list(plos=plos_out, bmc=bmc_out)
  class(res) <- "ft"
  res
}

#' @method print ft
#' @export
#' @rdname ft_search
print.ft <- function(x, ...)
{
  cat(sprintf("Query [%s]", x[[1]]$opts$q), "\n")
  cat(sprintf("No. records found [%s]", x[[1]]$found), "\n")
  cat(sprintf("No. records returned [%s]", NROW(x[[1]]$data)), "\n")
#   cat(sprintf("First record abbrev [format:%s] .\n\n", x$format))
  cat("Data: \n\n")
  if(is(x[[1]]$data, "data.frame")) print(x[[1]]$data) else cat("no data")
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