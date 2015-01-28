#' Collect data from a remote source in fulltext
#' 
#' @name collect
#' @export
#' @param x Input.
#' @param ... Further args, ignored.
#' @examples \dontrun{
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")
#' x %>% collect()
#' x %>% collect() %>% text()
#' }
collect <- function(x, ...) UseMethod("collect")

#' @export
#' @rdname collect
collect.ft_data <- function(x, ...){
  for(i in seq_along(x)){
    x[[i]]$data$data <- cache_get(key=x[[i]]$data$path, 
                                  backend=cache_options_get()$backend, 
                                  path=cache_options_get()$path)
  }
  x
}

text <- function(x, ...) UseMethod("text")
text.ft_data <- function(x, ...){
  lapply(x, function(z){
    unclass(z$data$data)
  })
}
