#' Extract chunks of data from articles.
#'
#' @export
#'
#' @param x Input article
#' @param what What to get, can be 1 to many. See Details.
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
#' x %>% 
#'  chunks(c("doi","publisher","permissions")) %>% 
#'  tabularize() %>% 
#'  .$plos %>% 
#'  select(-permissions.license)
#'  
#' x <- ft_get(c("10.3389/fnagi.2014.00130",'10.1155/2014/249309','10.1155/2014/162024'), 
#'    from='entrez')
#' x %>% chunks(c("doi","keywords")) %>% tabularize()
#' x %>% chunks("authors") %>% tabularize()
#' x %>% chunks(c("doi","publisher","permissions")) %>% tabularize()
#' x %>% chunks("history") %>% tabularize()
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
#' ft_get(res$entrez$data$DOI, from='entrez') %>% chunks("title")
#' ft_get(res$entrez$data$DOI[1:4], from='entrez') %>% chunks("acknowledgments")
#' ft_get(res$entrez$data$DOI[1:4], from='entrez') %>% chunks(c('title','keywords'))
#' 
#' # From eLife
#' x <- ft_get(c('10.7554/eLife.04251', '10.7554/eLife.04986'), from='elife')
#' x %>% chunks("abstract")
#' x %>% chunks("publisher")
#' x %>% chunks("journal_meta")
#' x %>% chunks("acknowledgments")
#' x %>% chunks("refs_dois")
#' x %>% chunks(c("abstract","executive_summary"))
#' }

chunks <- function(x, what='all') {
  is_ft_data(x)
  what <- match.arg(what, c("all", sections()), TRUE)
  out <- list()
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$found)) {
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <- 
      lapply(x[[i]]$data$data, function(q){
        qparsed <- xmlParse(q)
        get_what(data = qparsed, what, names(x[i]))
      })
    }
  }
  out
}

sections <- function() c("front","body","back","title","doi","categories","authors","keywords",
                         "abstract","executive_summary","refs","refs_dois",
                         "publisher","journal_meta","article_meta",
                         "acknowledgments","permissions","history")

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
         elife = xpathSApply(b, "//title-group/article-title", xmlValue)[[1]],
         plos = xpathSApply(b, "//title-group/article-title", xmlValue)[[1]],
         entrez = xpathSApply(b, "//title-group/article-title", xmlValue)[[1]]
  )
}

doi <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//article-id[@pub-id-type='doi']", xmlValue)[[1]],
         plos = xpathSApply(b, "//article-id[@pub-id-type='doi']", xmlValue)[[1]],
         entrez = xpathSApply(b, "//article-id[@pub-id-type='doi']", xmlValue)[[1]]
  )
}

categories <- function(b, from){
  switch(from, 
         elife = xpathSApply(xpathSApply(b, "//article-categories")[[1]], "//subject", xmlValue),
         plos = xpathSApply(xpathSApply(b, "//article-categories")[[1]], "//subject", xmlValue),
         entrez = xpathSApply(xpathSApply(b, "//article-categories")[[1]], "//subject", xmlValue)
  )
}

authors <- function(b, from){
  get_auth <- function(v){
    tmp <- xpathSApply(v, "//contrib[@contrib-type='author']")
    lapply(tmp, function(z){
      list(given_names = xpathSApply(z, "name/given-names", xmlValue),
           surname = xpathSApply(z, "name/surname", xmlValue))
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
         elife = xpathSApply(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd", xmlValue),
         plos = NULL,
         entrez = xpathSApply(b, "//kwd-group/kwd", xmlValue)
  )
}

body <- function(b, from){
  switch(from, 
         elife = {
           body <- getNodeSet(b, "//body/p")
           body2 <- lapply(body, xmlValue)
           setNames(body2, sapply(body, xmlGetAttr, name = "hwp:id"))
         },
         plos = {
           body <- getNodeSet(b, "//body//p")
           lapply(body, xmlValue)
         }
  )
}

abstract <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//abstract[@hwp:id='abstract-1']/p", xmlValue)[[1]],
         plos = xpathSApply(b, "//abstract", xmlValue),
         entrez = xpathSApply(b, "//abstract", xmlValue)
  )
}

exec_summary <- function(b, from){
  switch(from, 
         elife = paste0(xpathSApply(b, "//abstract[@hwp:id='abstract-2']/p", xmlValue), collapse = " "),
         plos = NULL,
         entrez = NULL
  )
}

refs_dois <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//ref-list/ref//pub-id[@pub-id-type='doi']", xmlValue),
         plos = NULL,
         entrez = NULL
  )
}

refs <- function(b, from){
  switch(from, 
         elife = NULL,
         plos = xpathSApply(b, "//ref-list/ref/mixed-citation", xmlValue),
         entrez = xpathSApply(b, "//ref-list/ref", xmlValue)
  )
}

publisher <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//publisher")[[1]]),
         plos = xmlToList(xpathSApply(b, "//publisher")[[1]]),
         entrez = xmlToList(xpathSApply(b, "//publisher")[[1]])
  )
}

journal_meta <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//journal-meta")[[1]], addAttributes = TRUE),
         plos = xmlToList(xpathSApply(b, "//journal-meta")[[1]], addAttributes = TRUE),
         entrez = xmlToList(xpathSApply(b, "//journal-meta")[[1]], addAttributes = TRUE)
  )
}

article_meta <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//article-meta")[[1]], addAttributes = TRUE),
         plos = xmlToList(xpathSApply(b, "//article-meta")[[1]], addAttributes = TRUE),
         entrez = xmlToList(xpathSApply(b, "//article-meta")[[1]], addAttributes = TRUE)
  )
}

acknowledgments <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//ack/p", xmlValue),
         plos = xpathSApply(b, "//ack/p", xmlValue),
         entrez = xpathSApply(b, "//ack/p", xmlValue)
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
  tmp <- xmlToList(xpathSApply(v, "//permissions")[[1]], addAttributes = FALSE)
  tmp$license <- paste0(tmp$license[[1]], collapse = " ")
  lichref <- tryCatch(xmlGetAttr(xpathSApply(v, "//permissions/license//ext-link")[[1]], "xlink:href"), error = function(e) e)
  tmp$license_url <- if (is(lichref, "simpleError")) NA else lichref
  tmp
}

front <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//front")[[1]]),
         plos = xmlToList(xpathSApply(b, "//front")[[1]]),
         entrez = xmlToList(xpathSApply(b, "//front")[[1]])
  )
}

back <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//back")[[1]]),
         plos = xmlToList(xpathSApply(b, "//back")[[1]]),
         entrez = xmlToList(xpathSApply(b, "//back")[[1]])
  )
}

history <- function(b, from){
  switch(from, 
         elife = history2date(b),
         plos = history2date(b),
         entrez = history2date(b)
  )
}

history2date <- function(r){
  tmp <- xpathSApply(r, "//history/date")
  out <- lapply(tmp, function(rr){
    as.Date(paste0(sapply(c('day','month','year'), function(vv) xpathApply(rr, vv, xmlValue)), collapse = "-"), "%d-%m-%Y")
  })
  setNames(out, sapply(tmp, xmlGetAttr, name = "date-type"))
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
  lapply(out, rbind.fill)
}
