#' @title Collect data from a remote source in fulltext
#' 
#' @description `ft_collect` grabs full text data from a remote storage 
#' device. `ft_text` is a convenience function to grab the nested text 
#' data and bring it up in the list for easier access
#' 
#' @name collect
#' @export
#' @param x Input. An object of class \code{ft_data}
#' @param ... Further args, ignored.
#' 
#' @details Note that for xml or plain text the result of this call
#' is actual text you can read. For pdf the result is raw bytes - 
#' use [ft_extract()] to convert raw to text.
#' 
#' @examples \dontrun{
#' # Get some data, stash in rds file
#' cache_options_get()
#' cache_options_set(cache=TRUE, backend="rds")
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos')
#' 
#' # note that the data is not in the object, gives NULL
#' x$plos$data$data
#' 
#' # Collect data from the rds file
#' y <- x %>% ft_collect()
#' 
#' # note how the data is now in the object
#' y$plos$data$data
#' 
#' # Let's get the actual 
#' ## ft_collect() alone, replaces file pointers with parsed text, 
#' ##  maintaining object structure
#' x %>% ft_collect() 
#' ## pulls the text out of the object
#' x %>% ft_collect() %>% ft_text()
#' }
ft_collect <- function(x, ...) {
  UseMethod("ft_collect")
}

#' @export
#' @rdname collect
ft_collect.default <- function(x, ...) {
  stop("no 'ft_collect' method for ", class(x)[[1]], call. = FALSE)
}

#' @export
#' @rdname collect
ft_collect.ft_data <- function(x, ...) {
  for (i in seq_along(x)) {
    path <- x[[i]]$data$path
    x[[i]]$data$data <- lapply(path, function(z) {
      cache_get(
        key = z$path, 
        backend = cache_options_get()$backend, 
        path = cache_options_get()$path
      )
    })
  }
  return(x)
}

#' @export
#' @rdname collect
ft_text <- function(x, ...) {
  UseMethod("ft_text")
}

#' @export
#' @rdname collect
ft_text.default <- function(x, ...) {
  stop("no 'ft_text' method for ", class(x)[[1]], call. = FALSE)
}

#' @export
#' @rdname collect
ft_text.ft_data <- function(x, ...) {
  lapply(x, function(z) {
    unclass(z$data$data)
  })
}
