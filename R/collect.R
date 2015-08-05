#' @title Collect data from a remote source in fulltext
#' 
#' @description \code{collect} grabs full text data from a remote storage 
#' device. \code{get_text} is a convenience function to grab the nested text 
#' data and bring it up in the list for easier access
#' 
#' @name collect
#' @export
#' @param x Input. An object of class \code{ft_data}
#' @param ... Further args, ignored.
#' @examples \dontrun{
#' # Get some data, stash in rds file
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")
#' 
#' # note that the data is not in the object, gives NULL
#' x$plos$data$data
#' 
#' # Collect data from the rds file
#' y <- x %>% collect()
#' 
#' # note how the data is now in the object
#' y$plos$data$data
#' 
#' # Let's get the actual 
#' x %>% collect() %>% get_text()
#' }
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @export
#' @rdname collect
collect.ft_data <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]]$data$data <- cache_get(key = x[[i]]$data$path, 
                                  backend = cache_options_get()$backend, 
                                  path = cache_options_get()$path)
  }
  x
}

#' @export
#' @rdname collect
get_text <- function(x, ...) {
  UseMethod("get_text")
}

#' @export
#' @rdname collect
get_text.ft_data <- function(x, ...) {
  lapply(x, function(z) {
    unclass(z$data$data)
  })
}
