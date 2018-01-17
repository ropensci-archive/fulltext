#' @title Get full text
#'
#' @description `ft_get` is a one stop shop to fetch full text of articles,
#' either XML or PDFs. We have specific support for PLOS via the
#' \pkg{rplos} package, Entrez via the \pkg{rentrez} package, and arXiv via the
#' \pkg{aRxiv} package. For other publishers, we have helpers to `ft_get` to
#' sort out links for full text based on user input. See `Details` for
#' help on how to use this function.
#'
#' @export
#'
#' @param x Either identifiers for papers, either DOIs (or other ids) as a list of
#' character strings, or a character vector, OR an object of class `ft`, as
#' returned from [ft_search()]
#' @param from Source to query. Optional.
#' @param type (character) one of xml (default), pdf, or plain (Elsevier only).
#' We choose to go with xml as the default as it has structure that a machine 
#' can reason about, but you are of course free to try to get xml, pdf, or plain
#' (in the case of Elsevier).
#' @param try_unknown (logical) if publisher plugin not already known, we try to 
#' fetch full text link either from ftdoi.org or from Crossref. If not found at
#' ftdoi.org or at Crossref we skip with a warning. If found with ftdoi.org or 
#' Crossref we attempt to download. Only applicable in `character` and `list`
#' S3 methods. Default: `TRUE`
#' @param plosopts PLOS options. See [rplos::plos_fulltext()]
#' @param bmcopts BMC options. parameter DEPRECATED
#' @param entrezopts Entrez options. See [rentrez::entrez_search()] and
#' [entrez_fetch()]
#' @param elifeopts eLife options
#' @param elsevieropts Elsevier options
#' @param crossrefopts Crossref options
#' @param wileyopts Wiley options
#' @param ... Further args passed on to [crul::HttpClient]
#'
#' @return An object of class `ft_data` (of type `S3`) with slots for
#' each of the publishers. The returned object is split up by publishers because
#' the full text format is the same within publisher - which should facilitate
#' text mining downstream as different steps may be needed for each publisher's
#' content.
#' 
#' Note that we have a print method for `ft_data` so you see something like this:
#' 
#' ```
#' <fulltext text>
#' [Docs] 4
#' [Source] ext - /Users/foobar/Library/Caches/R/fulltext
#' [IDs] 10.2307/1592482 10.2307/1119209 10.1037/11755-024 ...
#' ```
#' 
#' Within each publisher there is a list with the elements:
#' 
#' - `found`: number of full text articles found
#' - `dois`: the DOIs given and searched for
#' - `data`
#'     - `backend`: the backend. right now only `ext` for "by file extension",
#'       we may add other backends in the future, thus we retain this
#'     - `cache_path`: the base directory path for file caching
#'     - `path`: if file retrieved the full path to the file. if file not 
#'       retrived this is `NULL`
#'     - `data`: if text extracted (see [ft_collect()]) the text will be here, 
#'       but until then this is `NULL`
#' - `opts`: the options given like article type, dois
#'
#' @details There are various ways to use `ft_get`:
#' \itemize{
#'  \item Pass in only DOIs - leave `from` parameter `NULL`. This route will
#'  first query Crossref API for the publisher of the DOI, then we'll use the appropriate
#'  method to fetch full text from the publisher. If a publisher is not found for the DOI,
#'  then we'll throw back a message telling you a publisher was not found.
#'  \item Pass in DOIs (or other pub IDs) and use the `from` parameter. This route
#'  means we don't have to make an extra API call to Crossref (thus, this route is faster)
#'  to determine the publisher for each DOI. We go straight to getting full text based on
#'  the publisher.
#'  \item Use [ft_search()] to search for articles. Then pass that output to
#'  this function, which will use info in that object. This behaves the same as the previous
#'  option in that each DOI has publisher info so we know how to get full text for each
#'  DOI.
#' }
#'
#' Note that some publishers are available via Entrez, but often not recent 
#' articles, where "recent" may be a few months to a year or so. In that case, 
#' make sure to specify the publisher, or else you'll get back no data.
#' 
#' See **Rate Limits** and **Authentication** in 
#' [fulltext-package] for rate limiting and authentication information,
#' respectively
#' 
#' @section Notes on the `type` parameter:
#' Type is sometimes ignored, sometimes used. For certain data sources, 
#' they only accept one type. By data source/publisher:
#' 
#' - PLOS: pdf and xml
#' - Entrez: only xml
#' - eLife: pdf and xml
#' - Pensoft: pdf and xml
#' - arXiv: only pdf
#' - BiorXiv: only pdf
#' - Elsevier: xml and plain
#' - Wiley: only pdf
#' - Peerj: pdf and xml
#' - Informa: only pdf
#' - FrontiersIn: pdf and xml
#' - Copernicus: pdf and xml
#' - Scientific Societies: only pdf
#' - Crossref: depends on the publisher
#' - other data sources/publishers: there are too many to cover here - will 
#' try to make a helper in the future for what is covered by different 
#' publishers
#' 
#' @section How data is stored:
#' `ft_get` used to have many options for "backends". We have simplified this
#' to one option. That one option is that all full text files are written 
#' to disk on your machine. You can choose where these files are stored.
#' 
#' In addition, files are named by their IDs (usually DOIs), and the file 
#' extension for the full text type (pdf or xml usually). This makes inspecting
#' the files easy. 
#' 
#' @section Data formats: 
#' xml full text is stored in `.xml` files. pdf is stored in `.pdf` files. 
#' And plain text is stored in `.txt` files.
#' 
#' @section Reusing cached articles:
#' All files are written to disk and we check for a file matching the given
#' DOI/ID on each request - if found we use it and throw message saying so.
#' 
#' @section Caching:
#' Previously, you could set caching options in each `ft_get` function call. 
#' We've simplified this to only setting caching options through the function
#' [cache_options_set()] - and you can get your cache options using 
#' [cache_options_get()]. See those docs for help on caching.
#' 
#' @section Notes on specific publishers:
#' 
#' - arXiv: The IDs passed are not actually DOIs, though they look 
#'  similar. Thus, there's no way to not pass in the \code{from} parameter 
#'  as we can't determine unambiguously that the IDs passed in are from 
#'  arXiv.org.
#' - bmc: Is a hot mess since the Springer acquisition. It's been 
#'  removed as an officially supported plugin, some DOIs from them may 
#'  still work when passed in here, who knows, it's a mess.
#' 
#' @section Warnings:
#' You will see warnings thrown in the R shell or in the resulting object. 
#' See [ft_get-warnings] for more information on what warnings mean.
#'
#' @examples 
#' # List publishers included
#' ft_get_ls()
#' 
#' \dontrun{
#' # If you just have DOIs and don't know the publisher
#' ## PLOS
#' ft_get('10.1371/journal.pone.0086169')
#' 
#' ## PeerJ
#' ft_get('10.7717/peerj.228')
#' ft_get('10.7717/peerj.228', type = "pdf")
#' 
#' ## eLife
#' ### xml
#' ft_get('10.7554/eLife.03032')
#' res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), from = "elife")
#' res$elife
#' respdf <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), 
#'   from = "elife", type = "pdf")
#' respdf$elife
#' 
#' elife_xml <- ft_get('10.7554/eLife.03032', from = "elife")
#' library(magrittr)
#' elife_xml %<>% collect()
#' elife_xml$elife
#' ### pdf
#' elife_pdf <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'), 
#'   from = "elife", type = "pdf")
#' elife_pdf$elife
#' elife_pdf %<>% collect()
#' elife_pdf %>% ft_extract()
#' 
#' ## some BMC DOIs will work, but some may not, who knows
#' ft_get(c('10.1186/2049-2618-2-7', '10.1186/2193-1801-3-7'), from = "entrez")
#' 
#' ## FrontiersIn
#' res <- ft_get(c('10.3389/fphar.2014.00109', '10.3389/feart.2015.00009'))
#' res
#' res$frontiersin
#' 
#' ## Hindawi - via Entrez
#' res <- ft_get(c('10.1155/2014/292109','10.1155/2014/162024', '10.1155/2014/249309'))
#' res
#' res$hindawi
#' res$hindawi$data$path
#' res$hindawi$data$data
#' res %>% collect() %>% .$hindawi
#' 
#' ## F1000Research - via Entrez
#' x <- ft_get('10.12688/f1000research.6522.1')
#' ## Two different publishers via Entrez - retains publisher names
#' res <- ft_get(c('10.1155/2014/292109', '10.12688/f1000research.6522.1'))
#' res$hindawi
#' res$f1000research
#' 
#' ## Pensoft
#' ft_get('10.3897/mycokeys.22.12528')
#' ### you'll need to specify the publisher for a DOI from a recent publication
#' ft_get('10.3897/zookeys.515.9332', from = "pensoft")
#' 
#' ## Copernicus
#' out <- ft_get(c('10.5194/angeo-31-2157-2013', '10.5194/bg-12-4577-2015'))
#' out$copernicus
#' 
#' ## arXiv - only pdf, you have to pass in the from parameter
#' res <- ft_get(x='cond-mat/9309029', from = "arxiv")
#' res$arxiv
#' res %>% ft_extract  %>% .$arxiv
#' 
#' ## bioRxiv - only pdf
#' res <- ft_get(x='10.1101/012476')
#' res$biorxiv
#' 
#' ## Karger Publisher
#' (x <- ft_get('10.1159/000369331'))
#' x$karger
#' 
#' ## MDPI Publisher
#' (x <- ft_get('10.3390/nu3010063'))
#' x$mdpi
#' ft_get('10.3390/nu7085279')
#' ft_get(c('10.3390/nu3010063', '10.3390/nu7085279')) # not working, only getting 1
#' 
#' # Scientific Societies
#' ## this is a paywall article, you may not have access or you may
#' x <- ft_get("10.1094/PHYTO-04-17-0144-R")
#' 
#' # Informa
#' x <- ft_get("10.1080/03088839.2014.926032")
#' ft_get("10.1080/03088839.2013.863435")
#' 
#' ## CogentOA - part of Inform/Taylor Francis now
#' ft_get('10.1080/23311916.2014.938430')
#'
#' library(rplos)
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' ft_get(dois, from='plos')
#' ft_get(c('10.7717/peerj.228','10.7717/peerj.234'), from='entrez')
#'
#' # elife
#' ft_get('10.7554/eLife.04300', from='elife')
#' ft_get(c('10.7554/eLife.04300', '10.7554/eLife.03032'), from='elife')
#' ## search for elife papers via Entrez
#' dois <- ft_search("elife[journal]", from = "entrez")
#' ft_get(dois)
#'
#' # Frontiers in Pharmacology (publisher: Frontiers)
#' doi <- '10.3389/fphar.2014.00109'
#' ft_get(doi, from="entrez")
#'
#' # Hindawi Journals
#' ft_get(c('10.1155/2014/292109','10.1155/2014/162024','10.1155/2014/249309'), from='entrez')
#' res <- ft_search(query='ecology', from='crossref', limit=50,
#'                  crossrefopts = list(filter=list(has_full_text = TRUE,
#'                                                  member=98,
#'                                                  type='journal-article')))
#'
#' out <- ft_get(res$crossref$data$DOI[1:20], from='entrez')
#'
#' # Frontiers Publisher - Frontiers in Aging Nueroscience
#' res <- ft_get("10.3389/fnagi.2014.00130", from='entrez')
#' res$entrez
#'
#' # Search entrez, get some DOIs
#' (res <- ft_search(query='ecology', from='entrez'))
#' res$entrez$data$doi
#' ft_get(res$entrez$data$doi[1], from='entrez')
#' ft_get(res$entrez$data$doi[1:3], from='entrez')
#'
#' # Search entrez, and pass to ft_get()
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res)
#'
#' # elsevier, ugh
#' ## set an environment variable like Sys.setenv(CROSSREF_TDM = "your key")
#' ft_get(x = "10.1016/j.trac.2016.01.027", from = "elsevier")
#'
#' # wiley, ugh
#' ## Wiley has only PDF, so type parameter doesn't do anything
#' ft_get(x = "10.1006/asle.2001.0035", from = "wiley")
#' 
#' # IEEE, ugh
#' ft_get('10.1109/TCSVT.2012.2221191', type = "pdf")
#' 
#' # AIP Publishing
#' ft_get('10.1063/1.4967823', try_unknown = TRUE)
#' 
#' # PNAS
#' ft_get('10.1073/pnas.1708584115', try_unknown = TRUE)
#'
#' 
#' # From ft_links output
#' ## Crossref
#' (res2 <- ft_search(query = 'ecology', from = 'crossref', limit = 3))
#' (out <- ft_links(res2))
#' (ress <- ft_get(x = out, type = "pdf"))
#' ress$crossref
#' 
#' (x <- ft_links("10.1111/2041-210X.12656", "crossref"))
#' (y <- ft_get(x))
#'
#' ## PLOS
#' (res2 <- ft_search(query = 'ecology', from = 'plos', limit = 4))
#' (out <- ft_links(res2))
#' out$plos
#' (ress <- ft_get(x = out, type = "pdf"))
#' ress$plos
#' ress$plos$dois
#' ress$plos$data
#' ress$plos$data$path$`10.1371/journal.pone.0059813`
#' 
#' ## No publisher plugin provided yet
#' # ft_get('10.1037/10740-005')
#' ### but no link available for this DOI
#' res <- ft_get('10.1037/10740-005', try_unknown = TRUE)
#' res$crossref
#' ### a link IS available for this DOI
#' res <- ft_get('10.1037/10740-005', try_unknown = TRUE)
#' res$crossref
#' }

ft_get <- function(x, from = NULL, type = "xml", try_unknown = TRUE, plosopts = list(),
                   bmcopts = list(), entrezopts = list(), elifeopts = list(),
                   elsevieropts = list(), wileyopts = list(), 
                   crossrefopts = list(), ...) {
  UseMethod("ft_get")
}

#' @export
ft_get.default <- function(x, from=NULL, type = "xml", try_unknown = TRUE, plosopts=list(),
                           bmcopts=list(), entrezopts=list(), elifeopts=list(),
                           elsevieropts = list(), wileyopts = list(), 
                           crossrefopts = list(), ...){
  stop("no 'ft_get' method for ", class(x), call. = FALSE)
}

#' @export
ft_get.character <- function(x, from=NULL, type = "xml", try_unknown = TRUE, plosopts=list(),
                             bmcopts = list(), entrezopts=list(),
                             elifeopts=list(),
                             elsevieropts = list(), wileyopts = list(), 
                             crossrefopts = list(), ...) {
  check_type(type)
  check_cache()
  if (!is.null(from)) {
    from <- match.arg(from, c("plos", "entrez", "elife", "pensoft",
      "arxiv", "biorxiv", "elsevier", "wiley"))
    plos_out <- plugin_get_plos(from, x, plosopts, type, ...)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, type, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, type, ...)
    pensoft_out <- plugin_get_pensoft(from, x, list(), type, ...)
    arxiv_out <- plugin_get_arxiv(from, x, list(), type, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, list(), type, ...)
    els_out <- plugin_get_elsevier(from, x, elsevieropts, type, ...)
    wiley_out <- plugin_get_wiley(from, x, wileyopts, type, ...)
    structure(list(plos = plos_out, entrez = entrez_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out,
                   biorxiv = biorxiv_out, elsevier = els_out, 
                   wiley = wiley_out), class = "ft_data")
  } else {
    get_unknown(x, type, try_unknown, ...)
  }
}

#' @export
ft_get.list <- function(x, from=NULL, type = "xml", try_unknown = TRUE, plosopts=list(),
                        bmcopts = list(), entrezopts=list(), elifeopts=list(),
                        elsevieropts = list(), wileyopts = list(), 
                        crossrefopts = list(), ...) {
  check_type(type)
  check_cache()
  if (!is.null(from)) {
    from <- match.arg(from, c("plos", "entrez", "elife", "pensoft", 
      "arxiv", "biorxiv", "elsevier", "wiley"))
    plos_out <- plugin_get_plos(from, x, plosopts, type, ...)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, type, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, type, ...)
    pensoft_out <- plugin_get_pensoft(from, x, list(), type, ...)
    arxiv_out <- plugin_get_arxiv(from, x, list(), type, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, list(), type, ...)
    els_out <- plugin_get_elsevier(from, x, elsevieropts, type, ...)
    wiley_out <- plugin_get_wiley(from, x, wileyopts, type, ...)
    structure(list(plos = plos_out, entrez = entrez_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out,
                   biorxiv = biorxiv_out, elsevier = els_out, 
                   wiley = wiley_out), class = "ft_data")
  } else {
    get_unknown(x, type, try_unknown, ...)
  }
}

#' @export
ft_get.ft <- function(x, from=NULL, type = "xml", try_unknown = TRUE, plosopts=list(),
                      bmcopts=list(), entrezopts=list(), elifeopts=list(),
                      elsevieropts = list(), wileyopts = list(), 
                      crossrefopts = list(), ...) {

  check_type(type)
  check_cache()
  # warn on sources that aren't supported yet and will be skipped
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  not_supported <- c("elife", "pensoft", "bmc", "arxiv",
                     "biorxiv", "europmc", "scopus", 
                     "microsoft", "wiley")
  if (any(from %in% not_supported)) {
    message(sprintf(
      "the following not supported and will be skipped:\n  %s",
      paste0(not_supported[not_supported %in% from], collapse = ", ")
    ))
  }

  plos_out <- plugin_get_plos(from, x$plos$data$id, plosopts, type, ...)
  entrez_out <- plugin_get_entrez(from, x$entrez$data$doi,
                                  entrezopts, type, ...)
  cr_out <- NULL
  if ("crossref" %in% from) {
    crl <- ft_links(x$crossref$data$doi, from = "crossref")
    cr_out <- plugin_get_links_crossref(from, urls = crl$crossref$data,
                                        crossrefopts, type, ...)
  }
  structure(list(plos = plos_out, entrez = entrez_out,
                 crossref = cr_out), class = "ft_data")
}

#' @export
ft_get.ft_links <- function(x, from=NULL, type = "xml", try_unknown = TRUE, plosopts=list(),
                            bmcopts=list(), entrezopts=list(), elifeopts=list(),
                            elsevieropts = list(), wileyopts = list(), 
                            crossrefopts = list(), ...){

  check_type(type)
  check_cache()
  if (is.null(cache_options_get()$cache)) cache_options_set(FALSE)
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  plos_out <- plugin_get_links_plos(from, urls = x$plos$data,
                                    plosopts, type, ...)
  crossref_out <- plugin_get_links_crossref(from, urls = x$crossref$data,
                                            crossrefopts, type, ...)
  structure(list(plos = plos_out, crossref = crossref_out), class = "ft_data")
}


#' @export
#' @rdname ft_get
ft_get_ls <- function() {
  nms <- ls(getNamespace("fulltext"), all.names = TRUE, pattern = "plugin_get_")
  nms <- grep("_links|generator", nms, invert = TRUE, value = TRUE)
  gsub("plugin_get_", "", nms)
}





#' @export
print.ft_data <- function(x, ...) {
  cat("<fulltext text>", sep = "\n")
  alldois <- unlist(ft_compact(pluck(x, "dois")))
  alldois <- vapply(alldois, utils::URLdecode, "")
  namesprint <- paste(stats::na.omit(alldois[1:10]), collapse = " ")
  totgot <- sum(unlist(pluck(x, "found")))

  cat(sprintf("[Docs] %s", totgot), "\n")
  cat(sprintf("[Source] %s %s", print_backend(cache_options_get()$backend), expand_if(cache_options_get()$path)), "\n")
  cat(ft_wrap(sprintf("[IDs]\n %s ...", namesprint)), "\n")
}

ft_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, width = width)
  paste0(wrapped, collapse = "\n")
}

expand_if <- function(x) {
  if (!is.null(x)) paste0("- ", path.expand(x)) else ""
}

print_backend <- function(x) {
  if (!is.null(x)) x else "R session"
}



# get unknown from DOIs where from=NULL ------------------
get_unknown <- function(x, type, try_unknown, ...) {
  pubs <- Filter(function(z) !is.null(z) && length(z) > 0, 
    stats::setNames(lapply(x, get_publisher), x))
  df <- data.frame(pub = unlist(unname(pubs)), doi = names(pubs), 
    name = vapply(pubs, attr, '', 'publisher', USE.NAMES=FALSE),
    issn = vapply(pubs, attr, '', 'issn', USE.NAMES=FALSE),
    stringsAsFactors = FALSE)
  dfsplit <- split(df, df$pub)
  out <- list()
  url_pattern <- NULL
  for (i in seq_along(dfsplit)) {
    fun <- publisher_plugin(names(dfsplit)[i])
    if (is.null(fun)) {
      if (try_unknown) {
        tmp <- ftdoi_get(sprintf("api/members/%s/", names(dfsplit)[i]))

        if (tmp$status_code == 200) {
          fun <- plugin_get_links
          cont <- jsonlite::fromJSON(tmp$parse("UTF-8"), FALSE)
          if (cont$use_crossref_links) {
            # just use crossref links if true
            fun <- plugin_get_crossref
            tm_nm <- 'crossref'
            warning("no plugin for Crossref member ", names(dfsplit)[i], 
              " yet", call. = FALSE)
          } else {
            if (!is.null(cont$urls)) {
              url_pattern <- cont$urls
              tm_nm <- 'generic'
            } else {
              mtchs <- vapply(cont$journals, function(z) {
                any(unlist(z$issn) %in% strsplit(dfsplit[[i]]$issn, ",")[[1]])
              }, logical(1))
              if (!any(mtchs)) {
                fun <- plugin_get_crossref
                tm_nm <- 'crossref'
                warning("no plugin for Crossref member ", names(dfsplit)[i], 
                  " yet", call. = FALSE)
              } else {
                url_pattern <- cont$journals[mtchs][[1]]$urls
                tm_nm <- 'generic'
              }
            }
          }
        } else {
          fun <- plugin_get_crossref
          tm_nm <- 'crossref'
          warning("no plugin for Crossref member ", names(dfsplit)[i], 
            " yet", call. = FALSE)
        }

        pub_nm <- dfsplit[[i]]$name[1]
        out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), type, 
          url_pattern, ...)
      }
    } else {
      pub_nm <- get_pub_name(names(dfsplit)[i])
      tm_nm <- get_tm_name(names(dfsplit)[i])
      out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), type, url_pattern, ...)
    }
  }
  structure(out, class = "ft_data")
}

ftdoi_get <- function(path, opts = list()) {
  cli <- crul::HttpClient$new(
    url = "https://ftdoi.org", 
    headers = list(`User-Agent` = paste0('fulltext/', utils::packageVersion('fulltext'))),
    opts = opts
  )    
  cli$get(path)
}

publisher_plugin <- function(x) {
  switch(
    x,
    `4374` = plugin_get_elife,
    `340` = plugin_get_plos,
    `4443` = plugin_get_peerj,
    # `297` = plugin_get_bmc,
    `1965` = plugin_get_frontiersin,
    `98` = plugin_get_crossref,
    `4950` = plugin_get_entrez,
    `2258` = plugin_get_pensoft,
    `3145` = plugin_get_copernicus,
    `246` = plugin_get_biorxiv,
    `127` = plugin_get_entrez,
    # `301` = plugin_get_cogent,
    `1968` = plugin_get_entrez,
    `78` = plugin_get_elsevier,
    `311` = plugin_get_wiley,
    `1665` = plugin_get_scientificsocieties,
    `301` = plugin_get_informa,
    `292` = plugin_get_royalsocchem,
    `263` = plugin_get_ieee,
    `221` = plugin_get_aaas,
    `341` = plugin_get_pnas
  )
}

get_pub_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         # `297` = "bmc",
         `1965` = "frontiersin",
         `98` = "hindawi",
         `4950` = "f1000research",
         `2258` = "pensoft",
         `3145` = "copernicus",
         `246` = "biorxiv",
         `127` = "karger",
         # `301` = "cogent",
         `1968` = "mdpi",
         `78` = "elsevier",
         `311` = "wiley",
         `1665` = "scientificsocieties",
         `301` = "informa",
         `292` = "royalsocchem",
         `263` = "ieee",
         `221` = "aaas",
         `341` = "pnas",
         "crossref"
  )
}

get_tm_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         # `297` = "bmc",
         `1965` = "frontiersin",
         `98` = "crossref",
         `4950` = "entrez",
         `2258` = "pensoft",
         `3145` = "copernicus",
         `246` = "biorxiv",
         `127` = "entrez",
         # `301` = "cogent",
         `1968` = "entrez",
         `78` = "elsevier",
         `311` = "wiley",
         `1665` = "scientificsocieties",
         `301` = "informa",
         `292` = "royalsocchem",
         `263` = "ieee",
         `221` = "aaas",
         `341` = "pnas",
         "crossref"
  )
}

get_publisher <- function(x) {
  z <- rcrossref::cr_works(x)
  # FIXME: at some point replace this with 
  #   mapping of Crossref member number to a unique short name
  pub <- gsub("/|\\.|-|:|;|\\(|\\)|<|>|\\s", "_",  tolower(z$data$publisher))
  issn <- z$data$ISSN
  if (length(z) == 0) {
    NULL
  } else {
    id <- as.character(strextract(z$data$member, "[0-9]+"))
    attr(id, "publisher") <- pub %||% ""
    attr(id, "issn") <- issn %||% ""
    return(id)
  }
}

check_type <- function(x) {
  if (!is.character(x)) stop("'type' parameter must be character")
  if (!x %in% c('xml', 'pdf', 'plain')) {
    stop("'type' parameter must be 'xml', 'pdf', or 'plain'")
  }
}
