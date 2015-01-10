#' Collect data from a remote source in fulltext
#' 
#' @name collect
#' @param x Input.
#' @param ... Further args, ignored.
#' @examples \dontrun{
#' x <- ft_get(ids='10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")
#' x %>% collect()
#' }
collect <- function(x, ...) UseMethod("collect")

#' @export
#' @rdname collect
collect.ft_data <- function(x, ...){
  for(i in seq_along(x)){
    x[[i]]$data$data <- cache_get(x[[i]]$data$path, cache_options_get()$backend, cache_options_get()$path)
  }
  x
}
