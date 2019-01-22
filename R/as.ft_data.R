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
#' @seealso [ft_get()]
#' @details We use an internal store of identifiers to keep track of files.
#' These identifiers are in the output of [ft_get()] and you can see them 
#' in that output.  If a file does not have a matching entry in our index 
#' of files (e.g., if you drop a file into the cache location as in the 
#' example below), then we assign it an index based on the file path; we'd
#' ideally use an article DOI or similar but we can not safely retrieve it
#' with just a file path.
#' @examples
#' # put a file in the cache in case there aren't any
#' dir <- file.path(tempdir(), "testing")
#' dir.create(dir)
#' file <- system.file("examples", "elife.xml", package = "fulltext")
#' writeLines(readLines(file), tempfile(tmpdir = dir, fileext = ".xml"))
#' 
#' # call as.ft_data
#' x <- as.ft_data(path = dir)
#' 
#' # output lives underneath a special list index "cached" 
#' #   representing already present files
#' x$cached
#' 
#' \dontrun{
#' # collect chunks
#' if (requireNamespace("pubchunks")) {
#'   library(pubchunks)
#'   res <- ft_collect(x)
#'   pub_chunks(res, c("doi", "title")) %>% pub_tabularize()
#' }
#' }
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
    res <- tryCatch(
      fulltext_store$get(strsplit(basename(w), "\\.")[[1]][1]),
      error = function(e) e
    )
    if (inherits(res, "error")) digest::digest(w, "sha1") else res
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
    list(
      found = length(dat), 
      dois = names(dat$path), 
      data = dat, 
      opts = list(),
      error = data.frame(NULL)
    )
  )
}

ty_pe <- function(z) {
  strsplit(basename(z), "\\.")[[1]][2]
}
