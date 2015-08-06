#' @title Extract chunks of data from articles
#'
#' @description \code{chunks} makes it easy to extract sections of an article. You
#' can extract just authors across all articles, or all references sections, or
#' the complete text of each article. Then you can pass the output downstream for
#' vizualization and analysis.
#'
#' @export
#'
#' @param x An object of class \code{ft_data}, the output from a call to
#' \code{\link{ft_get}}
#' @param what What to get, can be one or more in a vector or list. See Details.
#'
#' @details Options for the \code{what} parameter:
#' \itemize{
#'  \item front - Publisher, journal and article metadata elements
#'  \item body - Body of the article
#'  \item back - Back of the article, acknowledgments, author contributions, references
#'  \item title - Article title
#'  \item doi - Article DOI
#'  \item categories - Publisher's categories, if any
#'  \item authors - Authors
#'  \item keywords - Keywords
#'  \item abstract - Article abstract
#'  \item executive_summary - Article executive summary
#'  \item refs - References
#'  \item refs_dois - References DOIs - if available
#'  \item publisher - Publisher name
#'  \item journal_meta - Journal metadata
#'  \item article_meta - Article metadata
#'  \item acknowledgments - Acknowledgments
#'  \item permissions - Article permissions
#'  \item history - Dates, recieved, published, accepted, etc.
#' }
#'
#' Note that we currently only support PLOS, eLife, and Entrez right now, more to come.
#'
#' @return A list of output, one for each thing requested
#' @examples \dontrun{
#' x <- ft_get('10.1371/journal.pone.0086169', from='plos')
#' chunks(x, what="authors")
#'
#' library("rplos")
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' x %>% chunks("front")
#' x %>% chunks("body")
#' x %>% chunks("back")
#' x %>% chunks("history")
#' x %>% chunks(c("doi","history")) %>% tabularize()
#' x %>% chunks("authors")
#' x %>% chunks(c("doi","categories"))
#' x %>% chunks("all")
#' x %>% chunks("publisher")
#' x %>% chunks("acknowledgments")
#' x %>% chunks("permissions")
#' x %>% chunks("journal_meta")
#' x %>% chunks("article_meta")
#'
#' # Coerce list output to a data.frame, where possible
#' (dois <- searchplos(q="*:*", fl='id',
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' x %>% chunks("publisher") %>% tabularize()
#' x %>% chunks("refs") %>% tabularize()
#' x %>% chunks(c("doi","publisher")) %>% tabularize()
#' x %>% chunks(c("doi","publisher","permissions")) %>% tabularize()
#'
#' x <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309','10.1155/2014/162024'),
#'    from='entrez')
#' x %>% chunks("doi") %>% tabularize()
#' x %>% chunks("authors") %>% tabularize()
#' x %>% chunks(c("doi","publisher","permissions")) %>% tabularize()
#' x %>% chunks("history") %>% tabularize()
#'
#' x <- ft_get('10.3389/fnagi.2014.00130', from='entrez')
#' x %>% chunks("keywords")
#'
#' # Piping workflow
#' opts <- list(fq=list('doc_type:full',"article_type:\"research article\""))
#' ft_search(query='ecology', from='plos', plosopts = opts)$plos$data$id %>%
#'  ft_get(from = "plos") %>%
#'  chunks("publisher")
#'
#' # Via entrez
#' res <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309','10.1155/2014/162024'),
#'    from='entrez')
#' chunks(res, what="abstract")
#' chunks(res, what="title")
#' chunks(res, what="keywords")
#' chunks(res, what="publisher")
#'
#' (res <- ft_search(query='ecology', from='entrez'))
#' ft_get(res$entrez$data$doi, from='entrez') %>% chunks("title")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>% chunks("acknowledgments")
#' ft_get(res$entrez$data$doi[1:4], from='entrez') %>% chunks(c('title','keywords'))
#'
#' # From eLife
#' x <- ft_get(c('10.7554/eLife.04251', '10.7554/eLife.04986'), from='elife')
#' x %>% chunks("abstract")
#' x %>% chunks("publisher")
#' x %>% chunks("journal_meta")
#' x %>% chunks("acknowledgments")
#' x %>% chunks("refs_dois")
#' x %>% chunks(c("abstract", "executive_summary"))
#' }

chunks <- function(x, what='all') {
  is_ft_data(x)
  what <- match.arg(unlist(what), c("all", sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <-
      lapply(x[[i]]$data$data, function(q){
        qparsed <- xml2::read_xml(q)
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
  setNames(lapply(what, function(z){
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
         entrez = f1txt(b, "//title-group/article-title")
  )
}

doi <- function(b, from){
  switch(from,
         elife = f1txt(b, "//article-id[@pub-id-type='doi']"),
         plos = f1txt(b, "//article-id[@pub-id-type='doi']"),
         entrez = f1txt(b, "//article-id[@pub-id-type='doi']")
  )
}

categories <- function(b, from){
  switch(from,
         elife = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         plos = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject")),
         entrez = xml2::xml_text(xml_find_all(xml2::xml_find_all(b, "//article-categories")[[1]], "//subject"))
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
  switch(from,
         elife = get_auth(b),
         plos = get_auth(b),
         entrez = get_auth(b)
  )
}

keywords <- function(b, from){
  switch(from,
         elife = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd")),
         plos = NULL,
         entrez = xml2::xml_text(xml2::xml_find_all(b, "//kwd-group/kwd"))
  )
}

body <- function(b, from){
  switch(from,
         elife = xml_text(xml_find_all(b, "//body//p")),
         plos = xml_text(xml_find_all(b, "//body//p"))
  )
}

abstract <- function(b, from){
  switch(from,
         elife = xml_text(xml_find_all(xml_find_all(b, '//abstract[@hwp:id="abstract-1"]', ns = xml_ns(b))[[1]], "p")[1]),
         plos = falltxt(b, "//abstract"),
         entrez = falltxt(b, "//abstract")
  )
}

exec_summary <- function(b, from){
  switch(from,
         elife = {
           tmp <- xml_text(xml_find_all(b, '//abstract[@abstract-type="executive-summary"]/p', ns = xml_ns(b)))
           tmp[-length(tmp)]
         },
         plos = NULL,
         entrez = NULL
  )
}

refs_dois <- function(b, from){
  switch(from,
         elife = falltxt(b, "//ref-list/ref//pub-id[@pub-id-type='doi']"),
         plos = NULL,
         entrez = NULL
  )
}

refs <- function(b, from){
  switch(from,
         elife = NULL,
         plos = falltxt(b, "//ref-list/ref/mixed-citation"),
         entrez = falltxt(b, "//ref-list/ref")
  )
}

publisher <- function(b, from){
  switch(from,
         elife = falltxt(b, "//publisher"),
         plos = falltxt(b, "//publisher"),
         entrez = falltxt(b, "//publisher")
  )
}

journal_meta <- function(b, from){
  switch(from,
         elife = lapply(xml2::xml_children(xml2::xml_find_one(b, "//journal-meta")), xml_node_parse),
         plos = lapply(xml2::xml_children(xml2::xml_find_one(b, "//journal-meta")), xml_node_parse),
         entrez = lapply(xml2::xml_children(xml2::xml_find_one(b, "//journal-meta")), xml_node_parse)
  )
}

article_meta <- function(b, from){
  switch(from,
         elife = lapply(xml2::xml_children(xml2::xml_find_one(b, "//article-meta")), xml_node_parse),
         plos = lapply(xml2::xml_children(xml2::xml_find_one(b, "//article-meta")), xml_node_parse),
         entrez = lapply(xml2::xml_children(xml2::xml_find_one(b, "//article-meta")), xml_node_parse)
  )
}

acknowledgments <- function(b, from){
  switch(from,
         elife = falltxt(b, "//ack/p"),
         plos = falltxt(b, "//ack/p"),
         entrez = falltxt(b, "//ack/p")
  )
}

permissions <- function(b, from){
  switch(from,
         elife = getperms(b),
         plos = getperms(b),
         entrez = getperms(b)
  )
}

getperms <- function(v){
  tmp <- sapply(xml2::xml_children(xml2::xml_find_one(v, "//permissions")), xml_node_parse)
  tmp$license <- paste0(tmp$license[[1]], collapse = " ")
  lichref <- tryCatch(xml2::xml_attr(xml2::xml_find_all(v, "//permissions/license//ext-link"), "href"), error = function(e) e)
  tmp$license_url <- if (is(lichref, "simpleError") || length(lichref) == 0) NA else lichref
  lapply(tmp, strtrim)
}

front <- function(b, from){
  switch(from,
         elife = get_forb(b, "//front"),
         plos = get_forb(b, "//front"),
         entrez = get_forb(b, "//front")
  )
}

back <- function(b, from){
  switch(from,
         elife = get_forb(b, "//back"),
         plos = get_forb(b, "//back"),
         entrez = get_forb(b, "//back")
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
         entrez = history2date(b)
  )
}

history2date <- function(r){
  tmp <- xml_find_all(r, "//history/date")
  out <- lapply(tmp, function(rr){
    as.Date(paste0(sapply(c('day','month','year'), function(vv) f1txt(rr, vv)), collapse = "-"), "%d-%m-%Y")
  })
  setNames(out, sapply(tmp, xml_attr, attr = "date-type"))
}

#' @export
#' @rdname chunks
tabularize <- function(x){
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
  vapply(xml2::xml_text(xml2::xml_find_one(x, xpath)), strtrim, "", USE.NAMES = FALSE)
}

falltxt <- function(x, xpath) {
  vapply(xml2::xml_text(xml2::xml_find_all(x, xpath)), strtrim, "", USE.NAMES = FALSE)
}

is_ft_data <- function(x) {
  if (!is(x, "ft_data")) stop("Input to x must be of class ft_data", call. = FALSE)
}
