#' Coerce directory of papers to ft_data object
#' 
#' create the same object that [ft_get()] outputs
#' from your cached files - without having to run
#' [ft_get()] again
#' 
#' @export
#' @param path cache path. if not given, we use the default
#' cache path. Default: `NULL`
#' @return an object of class `ft_data`
#' @examples
#' x <- as.ft_data()
#' # output lives underneath a special list index "cached" 
#' #   representing already present files
#' x$cached
#' 
#' y <- x %>% ft_collect()
#' y$cached$data$data
#' y %>% ft_chunks(what="title") %>% ft_tabularize()
#' x %>% ft_collect() %>% ft_text()
as.ft_data <- function(path = NULL) {
  if (is.null(path)) path <- ftxt_cache$cache_path_get()
  if (!dir.exists(path)) stop("path does not exist")  
  paths <- list.files(path, full.names = TRUE)
  z <- to_ft_data(paths)
  class(z) <- "ft_data"
  return(z)
}

to_ft_data <- function(x) {
  if (length(x) == 0) return(list(cached = null_list(list())))

  id <- unname(vapply(x, function(w) {
    fulltext_store$get(strsplit(basename(w), "\\.")[[1]][1])
  }, ""))

  dat <- list(path = stats::setNames(
    unname(Map(function(a, b, d) list(
      path = a,
      id = b,
      type = d,
      error = NULL
    ), x, id, unname(vapply(x, ty_pe, "")))),
    id
  ))

  list(cached = 
    list(found = length(dat), dois = names(dat$path), data = dat, opts = list())
  )
}

ty_pe <- function(z) {
  strsplit(basename(z), "\\.")[[1]][2]
}
