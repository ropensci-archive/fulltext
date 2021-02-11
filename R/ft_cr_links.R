#' Get Crossref full text links from a DOI
#'
#' @export
#' @param doi (character) A Digital Object Identifier (DOI). required.
#' @param type (character) One of 'xml', 'html', 'plain', 'pdf',
#' 'unspecified', or 'all' (default). required.
#' @param ... Named parameters passed on to [crul::HttpClient()]
#'
#' @details Note that this function is not vectorized.
#'
#' Some links returned will not in fact lead you to full text
#' content as you would understandbly think and expect. That is, if you
#' use the `filter` parameter with e.g., [rcrossref::cr_works()]
#' and filter to only full text content, some links may actually give back
#' only metadata for an article. Elsevier is perhaps the worst offender,
#' for one because they have a lot of entries in Crossref TDM, but most
#' of the links that are apparently full text are not in fact full text,
#' but only metadata. You can get full text if you are part of a subscribing
#' institution to that specific Elsever content, but otherwise, you're SOL.
#'
#' Note that there are still some bugs in the data returned form CrossRef.
#' For example, for the publisher eLife, they return a single URL with
#' content-type application/pdf, but the URL is not for a PDF, but for both
#' XML and PDF, and content-type can be set with that URL as either XML or
#' PDF to get that type.
#'
#' In another example, all Elsevier URLs at time of writing are have
#' `http` scheme, while those don't actually work, so we have a
#' custom fix in this function for that publisher. Anyway, expect changes...
#'
#' @section Register for the Polite Pool:
#' See of 'Authentication' setion of the [fulltext-package] manual page
#'
#' @return `NULL` if no full text links given; a list of tdmurl objects if
#' links found. a tdmurl object is an S3 class wrapped around a simple list,
#' with attributes for:
#'
#' - type: type, matchin type passed to the function
#' - doi: DOI
#' - member: Crossref member ID
#' - intended_application: intended application, e.g., text-mining
#'
#' @examples \dontrun{
#' dois <- c("10.1245/s10434-016-5211-6",
#' "10.17159/2413-3108/2016/v0i55a49", "10.17159/2413-3108/2015/v0i53a455",
#' "10.17159/2413-3108/2006/v0i18a982", "10.1007/s10665-016-9845-y", 
#' "10.1016/j.ad.2015.06.020", "10.1016/j.medipa.2014.03.002")
#'
#' # pdf link
#' ft_cr_links(doi = "10.5555/515151", "pdf")
#'
#' # xml and plain text links
#' ft_cr_links(dois[1], "pdf")
#' ft_cr_links(dois[6], "xml")
#' ft_cr_links(dois[7], "plain")
#' ft_cr_links(dois[1]) # all is the default
#'
#' # pdf link
#' ft_cr_links(doi = "10.5555/515151", "pdf")
#' ft_cr_links(doi = "10.3897/phytokeys.52.5250", "pdf")
#'
#' # many calls, use e.g., lapply
#' lapply(dois[1:3], ft_cr_links)
#'
#' # elsevier
#' ## DOI that is open acccess
#' ft_cr_links('10.1016/j.physletb.2010.10.049')
#' ## DOI that is not open acccess
#' ft_cr_links('10.1006/jeth.1993.1066')
#' }
ft_cr_links <- function(doi, type = 'all', ...) {
  res <- .crm_works_links(dois = doi, ...)[[1]]
  if (is.null(unlist(res$links))) {
    return(list())
  } else {
    elife <- grepl("elife", res$links[[1]]$URL)
    withtype <- if (type == 'all') {
      res$links
    } else {
      Filter(function(x) grepl(type, x$`content-type`), res$links)
    }

    if (is.null(withtype) || length(withtype) == 0) {
      return(list())
    } else {
      withtype <- stats::setNames(withtype, sapply(withtype, function(x){
        if (x$`content-type` == "unspecified") {
          "unspecified"
        } else {
          strsplit(x$`content-type`, "/")[[1]][[2]]
        }
      }))

      if (elife) {
        withtype <- Filter(function(w) !grepl("lookup", w$URL), withtype)
      }

      if (basename(res$member) %in% c("2258", "179")) {
        withtype <- lapply(withtype, function(z) {
          z$URL <- sub("http://", "https://", z$URL)
          z
        })
      }

      if (basename(res$member) == "78") {
        withtype <- lapply(withtype, function(z) {
          z$URL <- sub("http://", "https://", z$URL)
          z
        })
        pdf <- list(pdf =
          utils::modifyList(withtype[[1]],
            list(
              URL = sub("text/xml", "application/pdf", withtype[[1]]$URL),
              `content-type` = "application/pdf"
            )
        ))
        withtype <- c(withtype, pdf)
      }

      if (type == "all") {
        lapply(withtype, function(b) {
          makeurl(b$URL, st(b$`content-type`), doi, res$member, b$`intended-application`)
        })
      } else {
        y <- match.arg(type, c('xml', 'plain', 'html', 'pdf', 'unspecified'))
        makeurl(x = withtype[[y]]$URL, y = y, z = doi, res$member,
          withtype[[y]]$`intended-application`)
      }
    }
  }
}

.crm_works_links <- function(dois = NULL, ...) {
  get_links <- function(x, ...) {
    tmp <- crm_GET(sprintf("works/%s", x), NULL, FALSE, ...)
    trylinks <- tryCatch(tmp$message$link, error = function(e) e)
    if (inherits(trylinks, "error")) {
      NULL
    } else {
      list(links = trylinks, member = tmp$message$member)
    }
  }
  stats::setNames(lapply(dois, get_links, ...), dois)
}

st <- function(x){
  if (grepl("/", x)) {
    strsplit(x, "/")[[1]][[2]]
  } else {
    x
  }
}

crm_GET <- function(endpoint, args = list(), todf = TRUE, on_error = warning,
                    parse = TRUE, ...) {

  url <- sprintf("https://api.crossref.org/%s", endpoint)
  cli <- crul::HttpClient$new(
    url = url,
    headers = list(
      `User-Agent` = make_ua(),
      `X-USER-AGENT` = make_ua()
    ),
    opts = list(...)
  )
  res <- cli$get(query = args)
  doi <- gsub("works/|/agency|funders/", "", endpoint)
  if (!res$status_code < 300) {
    on_error(sprintf("%s: %s - (%s)", res$status_code, get_err(res), doi),
             call. = FALSE)
    list(message = NULL)
  } else {
    stopifnot(res$response_headers$`content-type` ==
                "application/json;charset=UTF-8")
    res <- res$parse("UTF-8")
    if (parse) jsonlite::fromJSON(res, todf) else res
  }
}

get_err <- function(x) {
  xx <- x$parse("UTF-8")
  if (x$response_headers$`content-type` == "text/plain") {
    tmp <- xx
  } else if (x$response_headers$`content-type` == "text/html") {
    html <- xml2::read_html(xx)
    tmp <- xml2::xml_text(xml2::xml_find_first(html, '//h3[@class="info"]'))
  } else if (
    x$response_headers$`content-type` == "application/json;charset=UTF-8"
  ) {
    tmp <- jsonlite::fromJSON(xx, FALSE)
  } else {
    tmp <- xx
  }
  if (inherits(tmp, "list")) {
    tmp$message[[1]]$message
  } else {
    if (any(class(tmp) %in% c("HTMLInternalDocument", "xml_document"))) {
      "Server error - check query - or api.crossref.org may have problems"
    } else {
      tmp
    }
  }
}
