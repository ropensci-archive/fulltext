#' Get abstracts
#'
#' @export
#' @param x (character) DOIs as a character vector
#' @param from Source to query. One or more of plos (default), scopus, or 
#' microsoft
#' @param plosopts PLOS options
#' @param scopusopts Scopus options
#' @param maopts Microsoft Academic options
#' @param ... curl options passed on to \code{\link[httr]{GET}}
#' @return An object of class \code{ft_abstract}
#' 
#' @section Authentication:
#' You need to use authentication for Scopus and Microsoft. Details:
#' 
#' \strong{Scopus}: Get a key at \url{https://dev.elsevier.com/index.html}, 
#' store it as an environment variable in your \code{.Renviron} file - 
#' see \code{\link{Startup}} for help.
#' 
#' Pass your API key into \code{scopusopts} as a named element in 
#' a list, e.g, \code{list(key = Sys.getenv('ELSEVIER_SCOPUS_KEY'))}
#' 
#' \strong{Microsoft}: Get a key by creating an account at 
#' \url{https://www.microsoft.com/cognitive-services/en-us/subscriptions}, then
#' requesting a key fir \strong{Academic}. Store it as an environment 
#' variable in your \code{.Renviron} file - see \code{\link{Startup}} for help.
#' 
#' Pass your API key into \code{maopts} as a named element in 
#' a list, e.g, \code{list(key = Sys.getenv('MICROSOFT_ACADEMIC_KEY'))}
#' 
#' \strong{PLOS}: none needed
#' 
#' @section Rate limits:
#' \strong{Scopus}: rate limits are: 10,000 per 7 days. See 
#' \url{https://dev.elsevier.com/api_key_settings.html} for rate 
#' limit information.
#' 
#' \strong{Microsoft}: rate limits are: 10,000 per month, and 1 per second.
#' 
#' \strong{PLOS}: There are no known rate limits for PLOS, though if you do 
#' hit something let me know.
#' @examples \dontrun{
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
#' out <- ft_abstract(x = dois[1], scopusopts = opts)
#' out
#' 
#' (out <- ft_abstract(x = dois[1:15], scopusopts = opts))
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
#' (res <- ft_search("Ti='ecology'...", from = "microsoft", 
#'   maopts = list(key = key)))
#' ids <- res$ma$data$Id
#' (out <- ft_abstract(x = ids, from = "microsoft",
#'   maopts = list(
#'     key = Sys.getenv('MICROSOFT_ACADEMIC_KEY')
#'   )
#' ))
#' out$ma
#' cat(unlist(lapply(out$ma, "[[", "abstract")), sep = "\n\n")
#' }
ft_abstract <- function(x, from = "plos", plosopts = list(), 
                        scopusopts = list(), maopts = list(), ...) {
  UseMethod("ft_abstract")
}

#' @export
ft_abstract.default <- function(x, from = "plos", plosopts = list(), 
                                scopusopts = list(), maopts = list(), ...) {
  stop("'ft_abstract' does not suport class ", class(x), call. = FALSE)
}

#' @export
ft_abstract.numeric <- function(x, from = "plos", plosopts = list(), 
                                scopusopts = list(), maopts = list(), ...) {
  ft_abstract(as.character(x), from, plosopts, scopusopts, maopts, ...)
}

#' @export
ft_abstract.character <- function(x, from = "plos", plosopts = list(), 
                                  scopusopts = list(), maopts = list(), ...) {
  from <- match.arg(from, c("plos", "scopus", "microsoft"))
  plos_out <- plugin_abstract_plos(from, x, plosopts)
  scopus_out <- plugin_abstract_scopus(from, x, scopusopts)
  ma_out <- plugin_abstract_microsoft(from, x, maopts)
  structure(list(plos = plos_out, scopus = scopus_out, ma = ma_out), 
            class = "ft_abstract")
}

#' @export
print.ft_abstract <- function(x, ...) {
  cat("<fulltext abstracts>", sep = "\n")
  cat("Found:\n")
  cat(paste(
    sprintf("  [PLOS: %s", len_abs(x$plos)),
    sprintf("Scopus: %s", len_abs(x$scopus)),
    sprintf("Microsoft: %s]", len_abs(x$ma)),
    sep = "; "), "\n")
}

len_abs <- function(x) {
  tmp <- nchar(unlist(pluck(x, "abstract")))
  length(tmp[tmp > 0])
}
