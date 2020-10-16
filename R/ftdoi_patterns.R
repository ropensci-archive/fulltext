#' Download patterns files
#' 
#' Does various checks to see if patterns files alrady downloaded,
#' out of date, if some/all are deleted and in need of an update
#' @export
#' @family ftdoi
#' @return character vector of file paths
ftd_fetch_patterns <- function() {
  patterns_grab()
  list.files(file.path(ftdoi_cache$cache_path_get(), "patterns"))
}

latest_tag <- function() {
  rel_latest="https://api.github.com/repos/sckott/pubpatterns/releases/latest"
  jsonlite::fromJSON(ftd_GET(rel_latest))$tag_name
}
latest_tag_url <- function() {
  zip_url_pat=
  "https://github.com/sckott/pubpatterns/releases/download/%s/pubpatterns.zip"
  sprintf(zip_url_pat, latest_tag())
}
patterns_grab <- function() {
  ppath <- file.path(ftdoi_cache$cache_path_get(), "patterns")
  dir.create(ppath, showWarnings = FALSE, recursive = TRUE)
  zip_path <- file.path(ppath, "pubpatterns.zip")
  tag <- file.path(ppath, "tag.txt")
  manifest <- file.path(ppath, "manifest.txt")
  lt <- latest_tag()
  if (needs_update(lt, tag, manifest)) { # if tags don't match
    url <- latest_tag_url()
    ftd_GET_zip(url)
    cat(lt, sep = "\n", file = tag)
    cat(grep("\\.json", basename(ftdoi_cache$list()), value = TRUE),
      sep = "\n", file = manifest)
    utils::unzip(zip_path, exdir = ppath)
    unlink(zip_path)
  }
}
needs_update <- function(lt, tag, manifest) {
  if (!dir.exists(ftdoi_cache$cache_path_get())) return(TRUE)
  if (!file.exists(tag)) return(TRUE)
  if (!file.exists(manifest)) return(TRUE)
  up2date <- lt == readLines(tag) &&
    all(sort(readLines(manifest)) %in% 
      sort(grep("\\.json", basename(ftdoi_cache$list()), value = TRUE)))
  if (up2date) {
    message("already up to date")
    return(FALSE)
  }
  return(TRUE)
}
has_patterns <- function() {
  ppath <- file.path(ftdoi_cache$cache_path_get(), "patterns")
  jsonfiles <- list.files(ppath, pattern = ".json")
  dir.exists(ppath) && length(jsonfiles) > 10
}
