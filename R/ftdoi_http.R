make_ua <- function() {
  paste0("fulltext/", utils::packageVersion("fulltext"))
}

ftd_GET <- function(url, args = list(), ...) {
  cli <- crul::HttpClient$new(
    url = url,
    headers = list(`User-Agent` = make_ua())
  )
  res <- cli$get(query = args)
  errs(res)
  res$parse("UTF-8")
}

ftd_GET_zip <- function(url, ...) {
  ftdoi_cache$mkdir()
  con <- crul::HttpClient$new(url = url, opts = list(...))
  res <- con$get(
    disk = file.path(ftdoi_cache$cache_path_get(), "patterns/pubpatterns.zip"))
  res$raise_for_status()
}

errs <- function(x) {
  if (x$status_code > 201) {
    xx <- jsonlite::fromJSON(x$parse("UTF-8"))
    if ("error" %in% names(xx)) {
      # match by status code
      fun <- match_err(x$status_code)$new()
      fun$mssg <- xx$error
      fun$do_verbose(x)
    } else {
      # if no error message in response, just general stop
      fauxpas::http(x)
    }
  }
}

match_err <- function(code) {
  tmp <- paste0("fauxpas::",
                grep("HTTP*", getNamespaceExports("fauxpas"), value = TRUE))
  fxns <- lapply(tmp, function(x) eval(parse(text = x)))
  codes <- vapply(fxns, function(z) z$public_fields$status_code, 1)
  fxns[[which(code == codes)]]
}
