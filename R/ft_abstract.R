#' Get abstracts
#'
#' @export
#' @param x (character) DOIs as a character vector
#' @param from Source to query. only scopus for now
#' @param scopusopts Scopus options
#' @param maopts Microsoft Academic options
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @return An object of class \code{ft_abstract}
#' @examples \dontrun{
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
#' (out <- ft_abstract(x = ids[1:4], 
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
ft_abstract <- function(x, from = "scopus", scopusopts = list(), 
                        maopts = list(), ...) {
  UseMethod("ft_abstract")
}

#' @export
ft_abstract.default <- function(x, from = "scopus", scopusopts = list(), 
                                maopts = list(), ...) {
  stop("'ft_abstract' does not suport class ", class(x), call. = FALSE)
}

#' @export
ft_abstract.numeric <- function(x, from = "scopus", scopusopts = list(), 
                                maopts = list(), ...) {
  ft_abstract(as.character(x), from, scopusopts, maopts, ...)
}

#' @export
ft_abstract.character <- function(x, from = "scopus", scopusopts = list(),
                                  maopts = list(), ...) {
  from <- match.arg(from, c("scopus", "microsoft"))
  scopus_out <- plugin_abstract_scopus(from, x, scopusopts)
  ma_out <- plugin_abstract_microsoft(from, x, maopts)
  structure(list(scopus = scopus_out, ma = ma_out), 
            class = "ft_abstract", query = query)
}

#' @export
print.ft_abstract <- function(x, ...) {
  cat("<fulltext abstracts>", sep = "\n")
  
  cat(sprintf("Query:\n  [%s]", attr(x, "query")), "\n")
  
  cat("Found:\n")
  cat(paste(
    sprintf("  [Scopus: %s", len_abs(x$scopus)),
    sprintf("Microsoft: %s]", len_abs(x$ma)),
    sep = "; "), "\n")
}

len_abs <- function(x) {
  tmp <- nchar(unlist(pluck(x, "abstract")))
  length(tmp[tmp > 0])
}
