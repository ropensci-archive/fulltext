#' Get abstracts
#'
#' @export
#' @param x (character) DOIs as a character vector
#' @param from Source to query. only scopus for now
#' @param scopusopts eLife options
#' @param ... Further args passed on to \code{\link[httr]{GET}}
#' @return An object of class \code{ft_abstract} xxx
#' @examples \dontrun{
#' # Scopus
#' opts <- list(key = Sys.getenv('ELSEVIER_SCOPUS_KEY'))
#' ## search
#' (res <- ft_search(query = 'biology', from = 'scopus', scopusopts = opts, 
#'   limit = 25))
#' ## get abstract
#' dois <- na.omit(res$scopus$data$`prism:doi`)
#' out <- ft_abstract(x = dois[1], scopusopts = opts)
#' out
#' 
#' (out <- ft_abstract(x = dois[1:15], scopusopts = opts))
#' }
ft_abstract <- function(x, from = "scopus", scopusopts = list(),  ...) {
  UseMethod("ft_abstract")
}

#' @export
ft_abstract.default <- function(x, from = "scopus", scopusopts = list(),  ...) {
  stop("'ft_abstract' does not suport class ", class(x), call. = FALSE)
}

#' @export
ft_abstract.character <- function(x, from = "scopus", scopusopts = list(),  ...) {
  from <- match.arg(from, "scopus")
  scopus_out <- plugin_abstract_scopus(x, scopusopts)
  structure(list(scopus = scopus_out), class = "ft_abstract")
}

#' @export
print.ft_abstract <- function(x, ...) {
  cat("<fulltext abstracts>", sep = "\n")
  alldois <- unlist(pluck(x$scopus, "doi"))
  alldois <- vapply(alldois, URLdecode, "")
  namesprint <- paste(na.omit(alldois[1:10]), collapse = " ")
  cat("   first 10", "\n")
  for (i in 1:min(10, length(x$scopus))) {
    cat(sprintf("    %s: %s", 
                x$scopus[[i]]$doi, 
                substring(x$scopus[[i]]$abstract, 35, 100)), "\n")
  }
}
