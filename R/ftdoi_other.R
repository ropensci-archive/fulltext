#' Members
#' 
#' @keywords internal
#' @param id (character) a Crossref member ID. Default is `NULL` which
#' gets all members
#' @family ftdoi
#' @examples \dontrun{
#' ftd_members()
#' ftd_members(221)
#' ftd_members(1965)
#' 
#' # not found
#' # ftd_members(999)
#' }
ftd_members <- function(id = NULL) {
  if (is.null(id)) all_members() else a_member(id)
}

all_members <- function(id) {
  paths <- vapply(member_map, "[[", "", "path")
  lapply(file.path(ftdoi_cache$cache_path_get(), "patterns", paths),
    jsonlite::fromJSON)
}
a_member <- function(id) {
  x <- member_map[as.character(id)]
  if (is.null(x[[1]])) 
    stop('not a Crossref member or not supported yet', call.=FALSE)
  path <- file.path(ftdoi_cache$cache_path_get(), "patterns", x[[1]]$path)
  if (!file.exists(path)) stop(paste(path, ' does not exist'), call.=FALSE)
  jsonlite::fromJSON(path)
}

#' Prefixes
#' 
#' @keywords internal
#' @param id (character) a DOI prefix. Default is `NULL`, which 
#' gets all
#' @return named list of details of the publisher for the DOI prefix
#' @family ftdoi
#' @examples \dontrun{
#' ftd_prefixes()
#' ftd_prefixes(id = '10.1080')
#' 
#' # doesn't work
#' # ftd_prefixes(id = '10.9999')
#' }
ftd_prefixes <- function(id = NULL) {
  assert(id, "character")
  if (is.null(id)) all_prefixes() else a_prefix(id)
}

prefix_map <- list(
  "10.1080" = list(
    "name" = "cogent",
    "path" = "cogent.json"
  ),
  "10.2139" = list(
    "name" = "ssrn",
    "path" = "ssrn.json"
  )
)
all_prefixes <- function(id) {
  paths <- vapply(prefix_map, "[[", "", "path")
  lapply(file.path(ftdoi_cache$cache_path_get(), "patterns", paths),
    jsonlite::fromJSON)
}
a_prefix <- function(id) {
  x <- prefix_map[as.character(id)]
  if (is.null(x[[1]])) 
    stop('not a DOI prefix or not supported yet', call.=FALSE)
  path <- file.path(ftdoi_cache$cache_path_get(), "patterns", x[[1]]$path)
  if (!file.exists(path)) stop(paste(path, ' does not exist'), call.=FALSE)
  jsonlite::fromJSON(path)
}

#' prefix local
#' @keywords internal
#' @param doi (characte) a doi
#' @return a named list with: prefix, member, name
#' @family ftdoi
#' @examples \dontrun{
#' prefix_local('10.3390/ani4010082')
#' }
prefix_local <- function(doi) {
  prefix <- doi_prefix(doi)
  z <- crossref_member_prefix[crossref_member_prefix$prefixes %in% prefix,]
  list(prefix = prefix, member = as.character(z$id),
    name = z$primary_name)
}

