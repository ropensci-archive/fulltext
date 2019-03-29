#' Get abstracts
#'
#' @export
#' @param x (character) DOIs as a character vector
#' @param from Source to query. One or more of plos (default),
#' scopus, microsoft, or crossref
#' @param plosopts PLOS options, a named list.
#' @param scopusopts Scopus options, a named list.
#' @param maopts Microsoft Academic options, a named list.
#' @param crossrefopts Crossref options, a named list.
#' @param ... curl options passed on to [crul::HttpClient], see
#' examples below
#' @return An object of class `ft_abstract`
#' @details See **Rate Limits** and **Authentication** in
#' [fulltext-package] for rate limiting and authentication information,
#' respectively
#'
#' @examples
#' # List publishers included
#' ft_abstract_ls()
#'
#' \dontrun{
#' # PLOS
#' ## search
#' (res <- ft_search(query = 'biology', from = 'plos', limit = 25,
#'    plosopts = list(fq = list('doc_type:full', '-article_type:correction',
#'                   '-article_type:viewpoints'))))
#' ## get abstracts
#' dois <- res$plos$data$id
#' (out <- ft_abstract(x = dois, from = "plos"))
#' out$plos
#'
#' # Scopus
#' opts <- list(key = Sys.getenv('ELSEVIER_SCOPUS_KEY'))
#'
#' ## search
#' (res <- ft_search(query = 'biology', from = 'scopus', scopusopts = opts,
#'   limit = 25))
#' ## get abstract
#' dois <- na.omit(res$scopus$data$`prism:doi`)
#' out <- ft_abstract(x = dois[1:3], from = "scopus", scopusopts = opts)
#' out
#' out$scopus
#'
#' (out <- ft_abstract(x = dois[1:5], from = "scopus", scopusopts = opts))
#'
#' # use scopus Ids
#' (res <- ft_search(query = 'biology', from = 'scopus', scopusopts = opts,
#'   limit = 50))
#' ids <- fulltext:::strextract(res$scopus$data$`dc:identifier`, "[0-9]+")
#' (out <- ft_abstract(x = ids[1:4], from = 'scopus',
#'   scopusopts = list(
#'     key = Sys.getenv('ELSEVIER_SCOPUS_KEY'),
#'     id_type = "scopus_id"
#'   )
#' ))
#'
#' # Microsoft
#' key <- Sys.getenv("MICROSOFT_ACADEMIC_KEY")
#' (res <- ft_search("Y=[2010, 2012)", from = "microsoft",
#'   maopts = list(key = key)))
#' ids <- res$ma$data$Id
#' (out <- ft_abstract(x = ids, from = "microsoft",
#'   maopts = list(
#'     key = Sys.getenv('MICROSOFT_ACADEMIC_KEY')
#'   )
#' ))
#' out$ma
#' cat(unlist(lapply(out$ma, "[[", "abstract")), sep = "\n\n")
#'
#' # Crossref
#' (res <- ft_search("ecology", from = "crossref",
#'   crossrefopts = list(filter = c(has_abstract = TRUE))))
#' ids <- res$crossref$data$doi
#' (out <- ft_abstract(x = ids, from = "crossref"))
#' out$crossref
#' 
#' # curl options
#' ft_abstract("10.2458/v17i1.21696", from = "crossref", verbose = TRUE)
#' ft_abstract("10.1371/journal.pcbi.1002487", from = "plos", verbose = TRUE)
#' }
ft_abstract <- function(x, from = "plos", plosopts = list(),
                        scopusopts = list(), maopts = list(),
                        crossrefopts = list(), ...) {
  UseMethod("ft_abstract")
}

#' @export
ft_abstract.default <- function(x, from = "plos", plosopts = list(),
                                scopusopts = list(), maopts = list(),
                                crossrefopts = list(), ...) {
  stop("'ft_abstract' does not suport class ", class(x), call. = FALSE)
}

#' @export
ft_abstract.numeric <- function(x, from = "plos", plosopts = list(),
                                scopusopts = list(), maopts = list(),
                                crossrefopts = list(), ...) {
  ft_abstract(as.character(x), from, plosopts, scopusopts, maopts, ...)
}

#' @export
ft_abstract.character <- function(x, from = "plos", plosopts = list(),
                                  scopusopts = list(), maopts = list(),
                                  crossrefopts = list(), ...) {

  from <- match.arg(from, c("plos", "scopus", "microsoft", "crossref"))
  plos_out <- plugin_abstract_plos(from, x, plosopts, ...)
  scopus_out <- plugin_abstract_scopus(from, x, scopusopts, ...)
  ma_out <- plugin_abstract_microsoft(from, x, maopts, ...)
  cr_out <- plugin_abstract_crossref(from, x, crossrefopts, ...)
  structure(list(plos = plos_out, scopus = scopus_out,
    ma = ma_out, crossref = cr_out), class = "ft_abstract")
}

#' @export
#' @rdname ft_abstract
ft_abstract_ls <- function() {
  nms <- ls(getNamespace("fulltext"), all.names = TRUE, pattern = "plugin_abstract_")
  gsub("plugin_abstract_", "", nms)
}


#' @export
print.ft_abstract <- function(x, ...) {
  cat("<fulltext abstracts>", sep = "\n")
  cat("Found:\n")
  cat(paste(
    sprintf("  [PLOS: %s", len_abs(x$plos)),
    sprintf("Scopus: %s", len_abs(x$scopus)),
    sprintf("Microsoft: %s", len_abs(x$ma)),
    sprintf("Crossref: %s]", len_abs(x$crossref)),
    sep = "; "), "\n")
}

len_abs <- function(x) {
  abs <- pluck(x, "abstract")
  abs <- vapply(abs, paste, "", collapse = " ")
  tmp <- nchar(unlist(abs))
  length(tmp[tmp > 0])
}
