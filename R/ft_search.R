#' Search for full text.
#' 
#' @export
#' @importFrom rplos searchplos plos_fulltext
#' @importFrom bmc bmc_search bmc_xml
#' @importFrom rcrossref cr_works
#' @importFrom plyr rbind.fill
#' @importFrom rentrez entrez_summary
#' 
#' @param query Query terms
#' @param from Source to query
#' @param limit Number of records to return.
#' @param plosopts PLOS options. See \code{?searchplos}
#' @param bmcopts BMC options. See \code{?bmc_search}
#' @param crossrefopts Crossref options. See \code{?cr_works}
#' @param entrezopts Entrez options. See \code{?entrez_search}
#' @param ... Further args passed on to \code{httr::GET}. Not working right now...
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
#' # BMC
#' (res <- ft_search(query='ecology', from='bmc'))
#' res$bmc
#' 
#' # Entrez
#' (res <- ft_search(query='ecology', from='entrez'))
#' res$entrez
#'  
#' # Plos and Crossref
#' (res <- ft_search(query='ecology', from=c('plos','crossref')))
#' res$plos
#' res$crossref
#' }

ft_search <- function(query, from='plos', limit=10, plosopts=list(), bmcopts=list(), 
  crossrefopts=list(), entrezopts=list(), ...)
{
  plos_out <- plugin_plos(from, query, limit, plosopts)
  bmc_out <- plugin_bmc(from, query, limit, bmcopts)
  cr_out <- plugin_crossref(from, query, limit, crossrefopts)
  en_out <- plugin_entrez(from, query, limit, entrezopts)
  res <- list(plos=plos_out, bmc=bmc_out, crossref=cr_out, entrez=en_out)
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
    sprintf("  [PLoS: %s", null_len(x$plos$found)), 
    sprintf("BMC: %s", null_len(x$bmc$found)), 
    sprintf("Crossref: %s", null_len(x$crossref$found)),
    sprintf("Entrez: %s]", null_len(x$entrez$found)),
    sep = "; "), "\n")
  
  cat("Returned:\n")
  cat(paste(
    sprintf("  [PLoS: %s", NROW(x$plos$data)), 
    sprintf("BMC: %s", NROW(x$bmc$data)), 
    sprintf("Crossref: %s", NROW(x$crossref$data)),
    sprintf("Entrez: %s]", NROW(x$entrez$data)),
    sep = "; "), "\n")
}

null_len <- function(x) if(is.null(x)) 0 else x

#' Print brief summary of ft_ind object
#'
#' @param x Input...
#' @param ... Ignored.
#' @param n Number of data frame rows to print
#' @method print ft_ind
#' @export

print.ft_ind <- function(x, ..., n = 10) {
  rows <- NROW(x$data)
  found <- x$found
  cat(sprintf("Query: [%s]", attr(x, "query")), "\n")
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
    structure(zz, class="ft_ind", query=query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class="ft_ind", query=query)
  }
}

plugin_crossref <- function(sources, query, limit, opts){
  if(any(grepl("crossref", sources))){
    opts$query <- query
    opts$limit <- limit
    out <- do.call(cr_works, opts)
    zz <- list(found = out$meta$`total-results`, data = out$data, opts = opts)
    structure(zz, class="ft_ind", query=query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class="ft_ind", query=query)
  }
}

plugin_bmc <- function(sources, query, limit, opts){
  if(any(grepl("bmc", sources))){
    opts$terms <- query
    opts$limit <- limit
    out <- do.call(bmc_search, opts)
    dat <- do.call(rbind, lapply(out@results$entries, data.frame, stringsAsFactors = FALSE))
    opts$query <- opts$terms; opts$terms <- NULL
    zz <- list(found = NA, data = dat, opts = opts)
    structure(zz, class="ft_ind", query=query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class="ft_ind", query=query)
  }
}

plugin_entrez <- function(sources, query, limit, opts){
  if(any(grepl("entrez", sources))){
    opts$db <- "pmc"
    opts$term <- query
    opts$retmax <- limit
    out <- do.call(entrez_search, opts)
    sumres <- entrez_summary(db = "pmc", id=out$ids)
    dat <- lapply(sumres, function(x){
      x$file <- NULL
      data.frame(lapply(x, function(z){
        tmp <- if(length(z) > 1) paste(z, collapse=", ") else z
        if(is.null(tmp)) NA else tmp
      }), stringsAsFactors=FALSE)
    })
    data <- do.call(rbind.fill, dat)
    data <- move_col(data, "Title")
    data <- move_col(data, "AuthorList")
    
    zz <- list(found = out$count, data = data, opts = opts)
    structure(zz, class="ft_ind", query=query)
  } else {
    zz <- list(found = NULL, data = NULL, opts = opts)
    structure(zz, class="ft_ind", query=query)
  }
}

move_col <- function(x, y) x[ c(names(x)[-grep(y, names(x))], y) ]
