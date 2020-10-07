#' @title Collect article text from local files
#' 
#' @description `ft_collect` grabs full text data from file paths in your 
#' `ft_data` object (result of call to `ft_get()`). `ft_text` is a 
#' convenience function to grab the nested text data and bring it up in 
#' the list for easier access
#' 
#' @export
#' @param x Input. An object of class `ft_data`
#' @param ... Further args, ignored.
#' 
#' @return an object of class `ft_data`, but the `data` slot should have
#' character string of text from the XML/plain text/PDF file
#' @details The result of this call is actual text you can read
#' 
#' @examples \dontrun{
#' # Get some data
#' x <- ft_get('10.1371/journal.pone.0086169')
#' 
#' # note that the data is not in the object, gives NULL
#' x$plos$data$data
#' 
#' # Collect data from the .xml file
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
ft_collect.default <- function(x, ...) {
  stop("no 'ft_collect' method for ", class(x)[[1]], call. = FALSE)
}

#' @export
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
#' @rdname ft_collect
ft_text <- function(x, ...) {
  UseMethod("ft_text")
}

#' @export
#' @rdname ft_collect
ft_text.default <- function(x, ...) {
  stop("no 'ft_text' method for ", class(x)[[1]], call. = FALSE)
}

#' @export
#' @rdname ft_collect
ft_text.ft_data <- function(x, ...) {
  lapply(x, function(z) {
    unclass(z$data$data)
  })
}
