#' @title Extract chunks of data from articles
#'
#' @description `ft_chunks` makes it easy to extract sections of an article.
#' You can extract just authors across all articles, or all references
#' sections, or the complete text of each article. Then you can pass the
#' output downstream for visualization and analysis.
#'
#' @export
#' @param x An object of class `ft_data`, the output from a call to
#' [ft_get()]
#' @param what What to get, can be one or more in a vector or list.
#' See Details.
#'
#' @details Options for the `what` parameter:
#' 
#' * front - Publisher, journal and article metadata elements
#' * body - Body of the article
#' * back - Back of the article, acknowledgments, author contributions,
#'  references
#' * title - Article title
#' * doi - Article DOI
#' * categories - Publisher's categories, if any
#' * authors - Authors
#' * keywords - Keywords
#' * abstract - Article abstract
#' * executive_summary - Article executive summary
#' * refs - References
#' * refs_dois - References DOIs - if available
#' * publisher - Publisher name
#' * journal_meta - Journal metadata
#' * article_meta - Article metadata
#' * acknowledgments - Acknowledgments
#' * permissions - Article permissions
#' * history - Dates, recieved, published, accepted, etc.
#'
#' Note that we currently only support PLOS, eLife, Entrez, and Elsevier
#' right now; more to come.
#'
#' @return A list of output, one for each thing requested
#' @examples \dontrun{
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos')
#' x %>% ft_collect %>% ft_chunks(what="authors")
#'
#' library("rplos")
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""),
#'      limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' x %>% ft_chunks("front")
#' x %>% ft_chunks("body")
#' x %>% ft_chunks("back")
#' x %>% ft_chunks("history")
#' x %>% ft_chunks(c("doi","history")) %>% ft_tabularize()
#' x %>% ft_chunks("authors")
#' x %>% ft_chunks(c("doi","categories"))
#' x %>% ft_chunks("all")
#' x %>% ft_chunks("publisher")
#' x %>% ft_chunks("acknowledgments")
#' x %>% ft_chunks("permissions")
#' x %>% ft_chunks("journal_meta")
#' x %>% ft_chunks("article_meta")
#'
#' # Coerce list output to a data.frame, where possible
#' dois <- c('10.7554/elife.28589', '10.7554/elife.14009', '10.7554/elife.13941', 
#'   '10.7554/elife.22170', '10.7554/elife.29285')
#' x <- ft_get(dois) 
#' x <- x %>% ft_collect()
#' x$elife
#' x %>% ft_chunks("publisher") %>% ft_tabularize()
#' x %>% ft_chunks("refs") %>% ft_tabularize()
#' x %>% ft_chunks(c("doi","publisher")) %>% ft_tabularize()
#' x %>% ft_chunks(c("doi","publisher","permissions")) %>% ft_tabularize()
#'
#' x <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309',
#'   '10.1155/2014/162024'), from='entrez')
#' x <- x %>% ft_collect()
#' x %>% ft_chunks("doi") %>% ft_tabularize()
#' x %>% ft_chunks("authors") %>% ft_tabularize()
#' x %>% ft_chunks(c("doi","publisher","permissions")) %>% ft_tabularize()
#' x %>% ft_chunks("history") %>% ft_tabularize()
#'
#' x <- ft_get('10.3389/fnagi.2014.00130', from='entrez')
#' x <- x %>% ft_collect()
#' x %>% ft_chunks("keywords")
#'
#' # Piping workflow
#' opts <- list(fq=list('doc_type:full',"article_type:\"research article\""))
#' ft_search(query='ecology', from='plos', plosopts = opts)$plos$data$id %>%
#'  ft_get(from = "plos") %>%
#'  ft_chunks("publisher")
#'
#' # Via entrez
#' res <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309',
#'    '10.1155/2014/162024'), from='entrez')
#' res <- res %>% ft_collect()
#' ft_chunks(res, what="abstract")
#' ft_chunks(res, what="title")
#' ft_chunks(res, what="keywords")
#' ft_chunks(res, what="publisher")
#'
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res$entrez$data$doi, from='entrez') %>% ft_collect() %>% ft_chunks("title")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>%
#'  ft_collect() %>% 
#'  ft_chunks("acknowledgments")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>%
#'  ft_collect() %>% 
#'  ft_chunks(c('title','keywords'))
#'
#' # From eLife
#' x <- ft_get(c('10.7554/eLife.04251', '10.7554/eLife.04986'), from='elife')
#' x %>% ft_chunks("abstract")
#' x %>% ft_chunks("publisher")
#' x %>% ft_chunks("journal_meta")
#' x %>% ft_chunks("acknowledgments")
#' x %>% ft_chunks("refs_dois")
#' x %>% ft_chunks(c("abstract", "executive_summary"))
#' }

ft_chunks <- function(x, what='all') {
  is_ft_data(x)
  what <- match.arg(unlist(what), c("all", sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      if (is.null(x[[i]]$data$data)) {
        warning("perhaps you need to run ft_collect()?")
      }
      out[[names(x[i])]] <-
      lapply(x[[i]]$data$data, function(q){
        qparsed <- if (inherits(q, "xml_document")) q else xml2::read_xml(q)
        get_what(data = qparsed, what, names(x[i]))
      })
    }
  }
  out
}

sections <- function() {
  c("front","body","back","title","doi","categories",
    "authors","keywords",
    "abstract","executive_summary","refs","refs_dois",
    "publisher","journal_meta","article_meta",
    "acknowledgments","permissions","history")
}

get_what <- function(data, what, from){
  if ( any(what == "all") ) what <- sections()
  stats::setNames(lapply(what, function(z){
    switch(z,
           front = front(data, from),
           body = body(data, from),
           back = back(data, from),
           title = title(data, from),
           abstract = abstract(data, from),
           executive_summary = exec_summary(data, from),
           doi = doi(data, from),
           categories = categories(data, from),
           authors = authors(data, from),
           keywords = keywords(data, from),
           refs_dois = refs_dois(data, from),
           refs = refs(data, from),
           publisher = publisher(data, from),
           journal_meta = journal_meta(data, from),
           article_meta = article_meta(data, from),
           acknowledgments = acknowledgments(data, from),
           permissions = permissions(data, from),
           history = history(data, from)
    )
  }), what)
}

title <- function(b, from){
  switch(from,
         elife = f1txt(b, "//title-group/article-title"),
         plos = f1txt(b, "//title-group/article-title"),
         entrez = f1txt(b, "//title-group/article-title"),
         elsevier = f1txt(b, "//dc:title")
  )
}

doi <- function(b, from){
  switch(from,
         elife = f1txt(b, "//article-id[@pub-id-type='doi']"),
         plos = f1txt(b, "//article-id[@pub-id-type='doi']"),
         entrez = f1txt(b, "//article-id[@pub-id-type='doi']"),
         elsevier = f1txt(b, "//dc:identifier")
  )
}

categories <- function(b, from){
  switch(from,
         elife = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         plos = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         entrez = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         elsevier = falltxt(b, "//dcterms:subject")
  )
}

authors <- function(b, from){
  get_auth <- function(v){
    tmp <- xml2::xml_find_all(v, "//contrib[@contrib-type='author']")
    lapply(tmp, function(z){
      list(given_names = f1txt(z, "name/given-names"),
           surname = f1txt(z, "name/surname"))
    })
  }
  switch(
    from,
    elife = get_auth(b),
    plos = get_auth(b),
    entrez = get_auth(b),
    elsevier = falltxt(b, "//dc:creator")
  )
}

keywords <- function(b, from){
  switch(
    from,
    elife = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd")),
    plos = NULL,
    entrez = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd")),
    elsevier = falltxt(b, "//ce:keyword")
  )
}

body <- function(b, from){
  switch(
    from,
    elife = xml_text(xml_find_all(b, "//body//p")),
    plos = xml_text(xml_find_all(b, "//body//p")),
    elsevier = {
      xml_ns_strip(b)
      falltxt(b, "//body")
    }
  )
}

abstract <- function(b, from){
  switch(
    from,
    elife = xml_text(xml_find_all(xml_find_all(b, '//abstract[@hwp:id="abstract-1"]', ns = xml_ns(b))[[1]], "p")[1]),
    plos = falltxt(b, "//abstract"),
    entrez = falltxt(b, "//abstract"),
    elsevier = f1txt(b, "//dc:description")
  )
}

exec_summary <- function(b, from){
  switch(
    from,
    elife = {
      tmp <- xml_text(xml_find_all(b, '//abstract[@abstract-type="executive-summary"]/p', ns = xml_ns(b)))
      tmp[-length(tmp)]
    },
    plos = NULL,
    entrez = NULL,
    elsevier = NULL
  )
}

refs_dois <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']"),
    plos = NULL,
    entrez = NULL,
    elsevier = NULL
  )
}

refs <- function(b, from){
  switch(
    from,
    elife = NULL,
    plos = falltxt(b, "//ref-list/ref/mixed-citation"),
    entrez = falltxt(b, "//ref-list/ref"),
    elsevier = falltxt(b, "//ce:bib-reference")
  )
}

publisher <- function(b, from){
  switch(
    from,
    elife = falltxt(b, "//publisher"),
    plos = falltxt(b, "//publisher"),
    entrez = falltxt(b, "//publisher"),
    elsevier = f1txt(b, "//prism:publisher")
  )
}

journal_meta <- function(b, from){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//journal-meta")), xml_node_parse),
    elsevier = NULL
  )
}

article_meta <- function(b, from){
  switch(
    from,
    elife = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    plos = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    entrez = lapply(xml2::xml_children(xml2::xml_find_first(b, "//article-meta")), xml_node_parse),
    elsevier = NULL
  )
}

acknowledgments <- function(b, from){
  switch(from,
         elife = falltxt(b, "//ack/p"),
         plos = falltxt(b, "//ack/p"),
         entrez = falltxt(b, "//ack/p"),
         elsevier = f1txt(b, "//ce:acknowledgment")
  )
}

permissions <- function(b, from){
  switch(from,
         elife = getperms(b),
         plos = getperms(b),
         entrez = getperms(b),
         elsevier = {
            tmp <- xml2::xml_find_first(b, "//xocs:copyright-info")
            list(
              copyright_text = f1txt(tmp, "xocs:cp-license-lines"),
              copyright_notice = f1txt(tmp, "xocs:cp-notices")
            )
         }
  )
}

getperms <- function(v){
  tmp <- sapply(xml2::xml_children(xml2::xml_find_first(v, "//permissions")), xml_node_parse)
  tmp$license <- paste0(tmp$license[[1]], collapse = " ")
  lichref <- tryCatch(xml2::xml_attr(xml2::xml_find_all(v, "//permissions/license//ext-link"), "href"), error = function(e) e)
  tmp$license_url <- if (inherits(lichref, "simpleError") || length(lichref) == 0) NA else lichref
  lapply(tmp, strtrim)
}

front <- function(b, from){
  switch(from,
         elife = get_forb(b, "//front"),
         plos = get_forb(b, "//front"),
         entrez = get_forb(b, "//front"),
         elsevier = NULL
  )
}

back <- function(b, from){
  switch(from,
         elife = get_forb(b, "//back"),
         plos = get_forb(b, "//back"),
         entrez = get_forb(b, "//back"),
         elsevier = NULL
  )
}

get_forb <- function(x, fb) {
  tmp <- xml_children(xml_find_all(x, fb))
  lapply(tmp, function(z) {
    lapply(xml_children(z), xml_node_parse)
  })
}

history <- function(b, from){
  switch(from,
         elife = history2date(b),
         plos = history2date(b),
         entrez = history2date(b),
         elsevier = NULL
  )
}

history2date <- function(r){
  tmp <- xml_find_all(r, "//history/date")
  out <- lapply(tmp, function(rr){
    as.Date(paste0(sapply(c('day','month','year'), function(vv) f1txt(rr, vv)), collapse = "-"), "%d-%m-%Y")
  })
  stats::setNames(out, sapply(tmp, xml_attr, attr = "date-type"))
}

#' @export
#' @rdname ft_chunks
ft_tabularize <- function(x){
  # each publisher
  out <- lapply(x, function(a){
    # each article
    lapply(a, function(y){
      y[sapply(y, length) == 0] <- NULL
      data.frame(y, stringsAsFactors = FALSE)
    })
  })
  lapply(out, rbind_fill)
}

f1txt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_first(x, xpath)), strtrim, "", USE.NAMES = FALSE)
}

falltxt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_all(x, xpath)), strtrim, "", USE.NAMES = FALSE)
}

is_ft_data <- function(x) {
  if (!inherits(x, "ft_data")) stop("Input to x must be of class ft_data", call. = FALSE)
}
