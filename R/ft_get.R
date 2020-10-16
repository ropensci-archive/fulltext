#' @title Download full text articles
#'
#' @description `ft_get` is a one stop shop to fetch full text of articles,
#' either XML or PDFs. We have specific support for PLOS via the
#' \pkg{rplos} package, Entrez via the \pkg{rentrez} package, and arXiv via the
#' \pkg{aRxiv} package. For other publishers, we have helpers to `ft_get` to
#' sort out links for full text based on user input. Articles are saved on
#' disk. See `Details` for help on how to use this function.
#'
#' @export
#'
#' @param x Either identifiers for papers, either DOIs (or other ids) as a
#' list of character strings, or a character vector, OR an object of class `ft`,
#' as returned from [ft_search()]
#' @param from Source to query. Optional.
#' @param type (character) one of xml (default), pdf, or plain (Elsevier
#' and ScienceDirect only). We choose to go with xml as the default as it has
#' structure that a machine can reason about, but you are of course free to try
#' to get xml, pdf, or plain (in the case of Elsevier and ScienceDirect).
#' @param try_unknown (logical) if publisher plugin not already known, we try
#' to fetch full text link either using \pkg{ftdoi} package or from Crossref.
#' If not found at \pkg{ftdoi} or at Crossref we skip with a warning.
#' If found with \pkg{ftdoi} or Crossref we attempt to download. Only
#' applicable in `character` and `list` S3 methods. Default: `TRUE`
#' @param bmcopts BMC options. parameter DEPRECATED
#' @param entrezopts Entrez options, a named list. See
#' [rentrez::entrez_search()] and [entrez_fetch()]
#' @param elifeopts eLife options, a named list.
#' @param elsevieropts Elsevier options, a named list. Use `retain_non_ft=TRUE`
#' to retain files that do not actually have full text but likely only have an
#' abstract. By default we set `retain_non_ft=FALSE` so that if we detect
#' that you only got an abstract back, we delete it and report an error
#' that you likely don't have access.
#' @param sciencedirectopts Elsevier ScienceDirect options, a named list.
#' @param crossrefopts Crossref options, a named list.
#' @param wileyopts Wiley options, a named list.
#' @param progress (logical) whether to show progress bar or not.
#' default: `FALSE`. if `TRUE`, we use `utils::txtProgressBar()` and
#' `utils::setTxtProgressBar()`
#' to create the progress bar; and each progress bar connection is closed
#' on function exit. A progress bar is run for each data source.
#' Works for all S3 methods except `ft_get.links`. When articles are not
#' already downloaded you see the progress bar. If articles are already
#' downloaded/cached, normally we throw messages saying so, but if a
#' progress bar is requested, then the messages are suppressed to
#' not interrupt the progress bar.
#' @param ... curl options passed on to [crul::HttpClient], see examples below
#'
#' @seealso [as.ft_data()]
#'
#' @return An object of class `ft_data` (of type `S3`) with slots for
#' each of the publishers. The returned object is split up by publishers
#' because the full text format is the same within publisher - which should
#' facilitate text mining downstream as different steps may be needed for
#' each publisher's content.
#'
#' Note that we have a print method for `ft_data` so you see something
#' like this:
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
#' - `errors`: data.frame of errors, with two columns for article id and error
#'
#' @details There are various ways to use `ft_get`:
#'
#' - Pass in only DOIs - leave `from` parameter `NULL`. This route will
#' first query Crossref API for the publisher of the DOI, then we'll use
#' the appropriate method to fetch full text from the publisher. If a publisher
#' is not found for the DOI, then we'll throw back a message telling you a
#' publisher was not found.
#' - Pass in DOIs (or other pub IDs) and use the `from` parameter. This route
#' means we don't have to make an extra API call to Crossref (thus, this route
#' is faster) to determine the publisher for each DOI. We go straight to
#' getting full text based on the publisher.
#' - Use [ft_search()] to search for articles. Then pass that output to
#'  this function, which will use info in that object. This behaves the same
#' as the previous option in that each DOI has publisher info so we know how to
#' get full text for each DOI.
#'
#' Note that some publishers are available via Entrez, but often not recent
#' articles, where "recent" may be a few months to a year or so. In that case,
#' make sure to specify the publisher, or else you'll get back no data.
#'
#' @section Important Access Notes:
#' See **Rate Limits** and **Authentication** in
#' [fulltext-package] for rate limiting and authentication information,
#' respectively.
#'
#' In particular, take note that when fetching full text from Wiley and
#' Elsevier, the only way that's done (unless it's one of their OA papers)
#' is through the Crossref TDM flow in which you need a Crossref TDM API
#' key and your institution needs to have access to the exact journal you
#' are trying to fetch a paper from. If your institution doesn't have
#' access you may still get a result, but likely its only the abstract.
#' Pretty much the same is true when fetching from ScienceDirect directly.
#' You need to have an Elsevier API key
#' that is valid for their TDM/article API.
#' See **Authentication** in [fulltext-package] for details.
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
#' - Elsevier ScienceDirect: xml and plain
#' - Wiley: pdf and xml
#' - Peerj: pdf and xml
#' - Informa: only pdf
#' - FrontiersIn: pdf and xml
#' - Copernicus: pdf and xml
#' - Scientific Societies: only pdf
#' - Cambridge: only pdf
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
#' # Collect all errors from across papers
#' #   similarly can combine from different publishers as well
#' res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.aaaa'), from = "elife")
#' res$elife$errors
#'
#' ## PeerJ
#' ft_get('10.7717/peerj.228')
#' ft_get('10.7717/peerj.228', type = "pdf")
#'
#' ## eLife
#' ### xml
#' ft_get('10.7554/eLife.03032')
#' res <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'),
#'   from = "elife")
#' res$elife
#' respdf <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'),
#'   from = "elife", type = "pdf")
#' respdf$elife
#'
#' elife_xml <- ft_get('10.7554/eLife.03032', from = "elife")
#' library(magrittr)
#' elife_xml %<>% ft_collect()
#' elife_xml$elife
#' ### pdf
#' elife_pdf <- ft_get(c('10.7554/eLife.03032', '10.7554/eLife.32763'),
#'   from = "elife", type = "pdf")
#' elife_pdf$elife
#' elife_pdf %<>% ft_collect()
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
#' res <- ft_get(c('10.1155/2014/292109','10.1155/2014/162024',
#' '10.1155/2014/249309'))
#' res
#' res$hindawi
#' res$hindawi$data$path
#' res %>% ft_collect() %>% .$hindawi
#'
#' ## F1000Research - via Entrez
#' x <- ft_get('10.12688/f1000research.6522.1')
#' ## Two different publishers via Entrez - retains publisher names
#' res <- ft_get(c('10.1155/2014/292109', '10.12688/f1000research.6522.1'))
#' res$hindawi
#' res$f1000research
#' 
#' ## Thieme -
#' ### coverage is hit and miss, it's not great
#' ft_get('10.1055/s-0032-1316462')
#'
#' ## Pensoft
#' ft_get('10.3897/mycokeys.22.12528')
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
#' ## AAAS - only pdf
#' res <- ft_get(x='10.1126/science.276.5312.548')
#' res$aaas
#' 
#' # The Royal Society
#' res <- ft_get("10.1098/rspa.2007.1849")
#' ft_get(c("10.1098/rspa.2007.1849", "10.1098/rstb.1970.0037",
#'   "10.1098/rsif.2006.0142"))
#'
#' ## Karger Publisher
#' (x <- ft_get('10.1159/000369331'))
#' x$karger
#'
#' ## MDPI Publisher
#' (x <- ft_get('10.3390/nu3010063'))
#' x$mdpi
#' ft_get('10.3390/nu7085279')
#' ft_get(c('10.3390/nu3010063', '10.3390/nu7085279'))
#'
#' # Scientific Societies
#' ## this is a paywall article, you may not have access or you may
#' x <- ft_get("10.1094/PHYTO-04-17-0144-R")
#' x$scientificsocieties
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
#'    fq=list('doc_type:full',"article_type:\"research article\""),
#'    limit=5)$data$id)
#' ft_get(dois)
#' ft_get(c('10.7717/peerj.228','10.7717/peerj.234'))
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
#' ft_get(c('10.1155/2014/292109','10.1155/2014/162024','10.1155/2014/249309'),
#'   from='entrez')
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
#' ### an open access article
#' ft_get(x = "10.1016/j.trac.2016.01.027", from = "elsevier")
#' ### non open access article
#' #### If you don't have access, by default you get abstract only, and we
#' ##### treat it as an error as we assume you want full text
#' ft_get(x = "10.1016/j.trac.2016.05.027", from = "elsevier")
#' #### If you want to retain whatever Elsevier gives you
#' ##### set "retain_non_ft = TRUE"
#' ft_get(x = "10.1016/j.trac.2016.05.027", from = "elsevier",
#'   elsevieropts = list(retain_non_ft = TRUE))
#'
#' # sciencedirect
#' ## set an environment variable like Sys.setenv(ELSEVIER_TDM_KEY = "your key")
#' ft_get(x = "10.1016/S0140-6736(13)62329-6", from = "sciencedirect")
#'
#' # wiley, ugh
#' ft_get(x = "10.1006/asle.2001.0035", from = "wiley", type = "pdf")
#' ## xml
#' ft_get(x = "10.1111/evo.13812", from = "wiley")
#' ## highwire fiasco paper
#' ft_get(x = "10.3732/ajb.1300053", from = "wiley")
#' ft_get(x = "10.3732/ajb.1300053", from = "wiley", type = "pdf")
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
#' # American Society for Microbiology
#' ft_get('10.1128/cvi.00178-17')
#'
#' # American Society of Clinical Oncology
#' ft_get('10.1200/JCO.18.00454')
#'
#' # American Institute of Physics
#' ft_get('10.1063/1.4895527')
#'
#' # American Chemical Society
#' ft_get(c('10.1021/la903074z', '10.1021/jp048806z'))
#' 
#' # Royal Society of Chemistry
#' ft_get('10.1039/c8cc06410e')
#'
#'
#' # From ft_links output
#' ## Crossref
#' (res2 <- ft_search(query = 'ecology', from = 'crossref', limit = 3,
#'   crossrefopts = list(filter = list(has_full_text=TRUE, member=98))))
#' (out <- ft_links(res2))
#' (ress <- ft_get(x = out, type = "pdf"))
#' ress$crossref
#'
#' (x <- ft_links("10.1111/2041-210X.12656", "crossref"))
#' (y <- ft_get(x))
#'
#' ## Cambridge
#' x <- ft_get("10.1017/s0922156598230305")
#' x$cambridge
#' z <- ft_get("10.1017/jmo.2019.20")
#' z$cambridge
#' m <- ft_get("10.1017/S0266467419000270")
#' m$cambridge
#'
#' ## No publisher plugin provided yet
#' ft_get('10.1037/10740-005')
#' ### no link available for this DOI
#' res <- ft_get('10.1037/10740-005', try_unknown = TRUE)
#' res[[1]]
#'
#' # Get a progress bar - off by default
#' library(rplos)
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""),
#'    limit=5)$data$id)
#' ## when articles not already downloaded you see the progress bar
#' b <- ft_get(dois, progress = TRUE)
#' ## if articles already downloaded/cached, normally we through messages
#' ## saying so
#' b <- ft_get(dois, progress = FALSE)
#' ## but if a progress bar is requested, then the messages are suppressed
#' b <- ft_get(dois, progress = TRUE)
#'
#' # curl options
#' ft_get("10.1371/journal.pcbi.1002487", verbose = TRUE)
#' ft_get('10.3897/mycokeys.22.12528', from = "pensoft", verbose = TRUE)
#' }

ft_get <- function(x, from = NULL, type = "xml", try_unknown = TRUE,
  bmcopts = list(), entrezopts = list(), elifeopts = list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  UseMethod("ft_get")
}

#' @export
ft_get.default <- function(x, from=NULL, type = "xml", try_unknown = TRUE,
  bmcopts=list(), entrezopts=list(), elifeopts=list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  stop("no 'ft_get' method for ", class(x), call. = FALSE)
}

#' @export
ft_get.character <- function(x, from=NULL, type = "xml", try_unknown = TRUE,
  bmcopts = list(), entrezopts=list(), elifeopts=list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  check_type(type)
  assert(progress, "logical")
  check_cache()
  if (!is.null(from)) {
    from <- match.arg(from, c("entrez", "elife", "pensoft",
      "arxiv", "biorxiv", "elsevier", "sciencedirect", "wiley"),
      several.ok = TRUE)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, type,
      progress = progress, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, type,
      progress = progress, ...)
    pensoft_out <- plugin_get_pensoft(from, x, list(), type,
      progress = progress, ...)
    arxiv_out <- plugin_get_arxiv(from, x, list(), type,
      progress = progress, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, list(), type,
      progress = progress, ...)
    els_out <- plugin_get_elsevier(from, x, elsevieropts, type,
      progress = progress, ...)
    sciencedirect_out <- plugin_get_sciencedirect(from, x, sciencedirectopts,
        type, progress = progress, ...)
    wiley_out <- plugin_get_wiley(from, x, wileyopts, type,
      progress = progress, ...)
    structure(list(entrez = entrez_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out,
                   biorxiv = biorxiv_out, elsevier = els_out,
                   sciencedirect = sciencedirect_out,
                   wiley = wiley_out), class = "ft_data")
  } else {
    get_unknown(x, type, try_unknown, progress, ...)
  }
}

#' @export
ft_get.list <- function(x, from=NULL, type = "xml", try_unknown = TRUE,
  bmcopts = list(), entrezopts=list(), elifeopts=list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  check_type(type)
  assert(progress, "logical")
  check_cache()
  if (!is.null(from)) {
    from <- match.arg(from, c("entrez", "elife", "pensoft",
      "arxiv", "biorxiv", "elsevier", "sciencedirect", "wiley"),
      several.ok = TRUE)
    entrez_out <- plugin_get_entrez(from, x, entrezopts, type,
      progress = progress, ...)
    elife_out <- plugin_get_elife(from, x, elifeopts, type,
      progress = progress, ...)
    pensoft_out <- plugin_get_pensoft(from, x, list(), type,
      progress = progress, ...)
    arxiv_out <- plugin_get_arxiv(from, x, list(), type,
      progress = progress, ...)
    biorxiv_out <- plugin_get_biorxiv(from, x, list(), type,
      progress = progress, ...)
    els_out <- plugin_get_elsevier(from, x, elsevieropts, type,
      progress = progress, ...)
    sciencedirect_out <- plugin_get_sciencedirect(from, x, sciencedirectopts,
      type, progress = progress, ...)
    wiley_out <- plugin_get_wiley(from, x, wileyopts, type,
      progress = progress, ...)
    structure(list(entrez = entrez_out, elife = elife_out,
                   pensoft = pensoft_out, arxiv = arxiv_out,
                   biorxiv = biorxiv_out, elsevier = els_out,
                   sciencedirect = sciencedirect_out,
                   wiley = wiley_out), class = "ft_data")
  } else {
    get_unknown(x, type, try_unknown, progress, ...)
  }
}

#' @export
ft_get.ft <- function(x, from=NULL, type = "xml", try_unknown = TRUE,
  bmcopts=list(), entrezopts=list(), elifeopts=list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  check_type(type)
  assert(progress, "logical")
  check_cache()
  # warn on sources that aren't supported yet and will be skipped
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  not_supported <- c("elife", "pensoft", "bmc", "arxiv",
                     "biorxiv", "europmc", "scopus",
                     "microsoft", "sciencedirect", "wiley")
  if (any(from %in% not_supported)) {
    message(sprintf(
      "the following not supported and will be skipped:\n  %s",
      paste0(not_supported[not_supported %in% from], collapse = ", ")
    ))
  }

  entrez_out <- plugin_get_entrez(from, x$entrez$data$doi,
    entrezopts, type, progress = progress, ...)
  cr_out <- NULL
  if ("crossref" %in% from) {
    crl <- ft_links(x$crossref$data$doi, from = "crossref")
    cr_out <- plugin_get_links_crossref(from, urls = crl$crossref$data,
                                        crossrefopts, type, ...)
  }
  structure(list(entrez = entrez_out,
                 crossref = cr_out), class = "ft_data")
}

#' @export
ft_get.ft_links <- function(x, from=NULL, type = "xml", try_unknown = TRUE,
  bmcopts=list(), entrezopts=list(), elifeopts=list(),
  elsevieropts = list(), sciencedirectopts = list(), wileyopts = list(),
  crossrefopts = list(), progress = FALSE, ...) {

  check_type(type)
  check_cache()
  if (is.null(cache_options_get()$cache)) cache_options_set(FALSE)
  from <- names(x[sapply(x, function(v) !is.null(v$data))])
  crossref_out <- plugin_get_links_crossref(from, urls = x$crossref$data,
                                            crossrefopts, type, ...)
  structure(list(crossref = crossref_out), class = "ft_data")
}


#' @export
#' @rdname ft_get
ft_get_ls <- function() {
  nms <- ls(getNamespace("fulltext"), all.names = TRUE,
    pattern = "plugin_get_")
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
  cat(sprintf("[Source] %s %s", print_backend(cache_options_get()$backend),
    expand_if(cache_options_get()$path)), "\n")
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
get_unknown <- function(x, type, try_unknown, progress = FALSE, ...) {
  # pubs <- Filter(function(z) !is.null(z) && length(z) > 0,
  #   stats::setNames(lapply(x, get_publisher, verbose = TRUE), x))
  pubs <- get_publisher2(x)
  df <- data.frame(pub = unlist(unname(pubs)), doi = names(pubs),
    name = vapply(pubs, attr, '', 'publisher', USE.NAMES=FALSE),
    issn = vapply(pubs, attr, '', 'issn', USE.NAMES=FALSE),
    error = vapply(pubs, attr, '', 'error', USE.NAMES=FALSE),
    stringsAsFactors = FALSE)
  dfsplit <- split(df, df$pub)
  out <- list()
  url_pattern <- NULL
  for (i in seq_along(dfsplit)) {
    fun <- publisher_plugin(names(dfsplit)[i])
    if (is.null(fun)) {
      if (try_unknown) {
        cont <- ftdoi_get(member = names(dfsplit)[i])

        if (!is.null(cont)) {
          fun <- plugin_get_links
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
          warning("no plugin for Crossref member '", names(dfsplit)[i],
            "' yet", call. = FALSE)
        }

        pub_nm <- dfsplit[[i]]$name[1]
        out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), type,
          url_pattern, progress = progress, ...)
      }
    } else {
      pub_nm <- get_pub_name(names(dfsplit)[i])
      tm_nm <- get_tm_name(names(dfsplit)[i])
      out[[ pub_nm ]] <- fun(tm_nm, dfsplit[[i]]$doi, list(), type,
        url_pattern, progress = progress, ...)
    }
  }
  structure(out, class = "ft_data")
}

ftdoi_get <- function(member) {
  res <- tryCatch(ftd_members(member), error = function(e) e)
  if (inherits(res, "error")) NULL else res
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
    `341` = plugin_get_pnas,
    `345` = plugin_get_microbiology,
    `10` = plugin_get_jama,
    `235` = plugin_get_amersocmicrobiol,
    `233` = plugin_get_amersocclinoncol,
    `8215` = plugin_get_instinvestfil,
    `317` = plugin_get_aip,
    `56` = plugin_get_cambridge,
    `237` = plugin_get_cob,
    `175` = plugin_get_roysoc
  )
}

get_pub_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         `297` = "springer",
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
         `345` = "microbiology",
         `10` = "jama",
         `235` = "amersocmicrobiol",
         `233` = "amersocclinoncol",
         `8215` = "instinvestfil",
         `317` = "aip",
         `179` = "sage",
         `2997` = "koreanacper",
         `1822` = "cdc",
         `56` = "cambridge",
         `237` = "cob",
         `175` = "roysoc",
         "crossref"
  )
}

get_tm_name <- function(x) {
  switch(x,
         `4374` = "elife",
         `340` = "plos",
         `4443` = "peerj",
         `297` = "springer",
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
         `345` = "microbiology",
         `10` = "jama",
         `235` = "amersocmicrobiol",
         `233` = "amersocclinoncol",
         `8215` = "instinvestfil",
         `317` = "aip",
         `56` = "cambridge",
         `237` = "cob",
         `175` = "roysoc",
         "crossref"
  )
}

# FIXME: pretty sure this isn't used anymore, delete if so
get_publisher <- function(x, ...) {
  z <- tryCatch(rcrossref::cr_works(x, ...), warning = function(w) w)
  # FIXME: at some point replace this with
  #   mapping of Crossref member number to a unique short name
  if (inherits(z, "warning")) return(unknown_id(z$message))
  pub <- gsub("/|\\.|-|:|;|\\(|\\)|<|>|\\s", "_",  tolower(z$data$publisher))
  names(z$data) <- tolower(names(z$data))
  issn <- z$data$issn
  if (length(z) == 0) {
    return(unknown_id(""))
  } else {
    id <- as.character(strextract(z$data$member, "[0-9]+"))
    attr(id, "publisher") <- pub %||% ""
    attr(id, "issn") <- issn %||% ""
    attr(id, "error") <- ""
    return(id)
  }
}

fat_cat_search_one <- function(dois, fields, size) {
  search_string <- make_doi_str(dois)
  cn <- crul::HttpClient$new("https://search.fatcat.wiki")
  query <- list(q = search_string, `_source` = paste0(fields, collapse = ","),
    size = size)
  res <- cn$get("fatcat_release/_search", query = query)
  res$raise_for_status()
  out <- jsonlite::fromJSON(res$parse("UTF-8"), flatten = TRUE)$hits$hits
  # if no data returned, make an empty data.frame
  if (length(out) == 0) {
    out <- data.frame(doi=NA_character_, container_issnl=NA_character_,
      container_name=NA_character_, publisher=NA_character_,
      stringsAsFactors=FALSE)
  }
  names(out) <- gsub("_source\\.", "", names(out))
  # strip out fields not contained in the result
  fields <- fields[fields %in% names(out)]
  # get just columns in fields
  df <- out[, fields]
  df$message <- rep(NA_character_, NROW(df))
  # add rows for DOIs not found
  doisstl <- tolower(sort(dois))
  not_found <- doisstl[!doisstl %in% tolower(sort(out$doi))]
  if (length(not_found) > 0) {
    for (i in not_found) df <- rbind(df, c(i, "", "", "", "not found"))
  }
  # remove rows with all NA
  df <- df[apply(df, 1, function(x) !all(is.na(x))), ]
  row.names(df) <- NULL
  return(df)
}

make_doi_str <- function(x) {
  sprintf("doi:(\"%s\")", paste0(x, collapse = "\" OR \""))
}

fat_cat_search <- function(dois, ...) {
  flds <- c('doi', 'container_issnl', 'container_name', 'publisher')

  if (length(dois) > 50) {
    chunk_size <- 50
    ids_chunked <- split(dois, ceiling(seq_along(dois)/chunk_size))
    out <- list()
    for (i in seq_along(ids_chunked)) {
      out[[i]] <- fat_cat_search_one(ids_chunked[[i]], flds,
        length(ids_chunked[[i]]))
    }
    res <- rbl(out)
  } else {
    res <- fat_cat_search_one(dois, flds, length(dois))
  }
  if (NROW(res) == 0) return(list())
  apply(res, 1, as.list)
}

get_publisher2 <- function(x, ...) {
  # crossref member ids, and publisher names
  pref <- strextract(x, "[0-9]{2}\\.[0-9]+")
  names(x) <- pref
  pref_uniq <- unique(pref)
  mems <- lapply(pref_uniq, function(z) {
    # tmp <- rcrossref::cr_prefixes(z)$data
    tmp <- prefix_local(z)
    list(
      prefix = z,
      member = basename(tmp$member),
      name = gsub("/|\\.|-|:|;|\\(|\\)|<|>|\\s", "_",  tolower(tmp$name)))
  })

  # issn's
  fc_res <- fat_cat_search(x)

  # put together
  out <- list()
  for (i in seq_along(x)) {
    prefix <- strextract(x[i], "[0-9]{2}\\.[0-9]+")
    mm <- mems[[which(prefix == vapply(mems, "[[", "", "prefix"))]]
    id <- mm$member
    fcm <- fc_res[[which(tolower(x[[i]]) == tolower(vapply(fc_res, "[[", "", "doi")))]]
    attr(id, "publisher") <- mm$name %||% ""
    attr(id, "issn") <- fcm$container_issnl %||% ""
    attr(id, "error") <- fcm$message
    out[[ x[[i]] ]] <- id
  }
  return(out)
}

unknown_id <- function(mssg) {
  id <- "unknown"
  attr(id, "publisher") <- "unknown"
  attr(id, "issn") <- "unknown"
  attr(id, "error") <- mssg
  return(id)
}

check_type <- function(x) {
  if (!is.character(x)) stop("'type' parameter must be character")
  if (!x %in% c('xml', 'pdf', 'plain')) {
    stop("'type' parameter must be 'xml', 'pdf', or 'plain'")
  }
}
