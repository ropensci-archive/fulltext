fulltext_store <- new.env()

#' Setup article caching
#' @export
#' @return a `storr` class, invisibly
#' @details the user isn't typically meant to use the output of this 
#' function. the main use of this function is to setup a new `storr`
#' directory for caching files for any [ft_get()] use. You can however
#' run this function and then interact with your storr cache yourself.
#' @examples \dontrun{
#' z <- ft_init()
#' class(z)
#' # z$destroy()
#' }
ft_init <- function() {
  path <- cache_options_get()$path
  if (is.null(path) || nchar(path) == 0) {
    stop('cache path not set, see ?cache_options_set', call. = FALSE)
  }
  tmp <- storr::storr_rds(paste0(path, "_storr"),
    mangle_key = TRUE)
  fulltext_store <<- tmp
  invisible(tmp)
}

check_fulltext_store <- function() {
  nf <- function() stop("run ft_init() first", call. = FALSE)
  if (!exists("fulltext_store")) nf()
  if (length(fulltext_store) == 0) nf()
  if (!dir.exists(fulltext_store$driver$path)) ft_init()
}
