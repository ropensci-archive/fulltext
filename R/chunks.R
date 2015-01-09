#' Extract chunks of data from articles.
#'
#' @export
#'
#' @param x Input article
#' @param what What to get, can be 1 to many. See Details.
#'
#' @details
#' \itemize{
#'  \item title
#'  \item doi
#'  \item categories
#'  \item authors
#'  \item keywords
#'  \item body
#'  \item abstract
#'  \item executive_summary
#'  \item refs
#'  \item refs_dois
#'  \item publisher
#'  \item journal_meta
#' }
#'
#' @return A list of output, one for each thing requested
#' @examples \dontrun{
#' x <- ft_get(ids=c('10.7554/eLife.04251','10.7554/eLife.04986'), from='elife')
#' chunks(x, what="abstract")
#' chunks(x, what="publisher")
#' chunks(x, what="journal_meta")
#' chunks(x, what="refs_dois")
#' chunks(x, what=c("abstract","executive_summary"))
#' 
#' x <- ft_get(ids='10.1371/journal.pone.0086169', from='plos')
#' chunks(x, what="authors")
#' 
#' library("rplos")
#' (dois <- searchplos(q="*:*", fl='id', 
#'    fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
#' x <- ft_get(dois, from="plos")
#' chunks(x, what="authors")
#' chunks(x, what=c("doi","categories"))
#' chunks(x, what="all")
#' chunks(x, what="publisher")
#' chunks(x, what="journal_meta")
#' 
#' # Piping workflow
#' opts <- list(fq=list('doc_type:full',"article_type:\"research article\""))
#' ft_search(query='ecology', from='plos', plosopts = opts)$plos$data$id %>% 
#'  ft_get(from = "plos") %>% 
#'  chunks("publisher")
#' }

chunks <- function(x, what='all') {
  what <- match.arg(what, c("all", sections()), TRUE)
  out <- list()
  for(i in seq_along(x)){
    if(is.null(x[[i]]$found)){
      out[[names(x[i])]] <- NULL
    } else {
      out[[names(x[i])]] <- 
      lapply(x[[i]]$data, function(q){
        qparsed <- xmlParse(q)
        get_what(data=qparsed, what, names(x[i]))
      })
    }
  }
  out
}

sections <- function() c("title","doi","categories","authors","keywords","body",
                         "abstract","executive_summary","refs","refs_dois",
                         "publisher","journal_meta")

get_what <- function(data, what, from){
  if( any(what == "all") ) what <- sections()
  setNames(lapply(what, function(z){
    switch(z,
           title = title(data, from),
           abstract = abstract(data, from),
           executive_summary = exec_summary(data, from),
           doi = doi(data, from),
           categories = categories(data, from),
           authors = authors(data, from),
           keywords = keywords(data, from),
           body = body(data, from),
           refs_dois = refs_dois(data, from),
           refs = refs(data, from),
           publisher = publisher(data, from),
           journal_meta = journal_meta(data, from)
    )
  }), what)
}

title <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//title-group/article-title", xmlValue)[[1]],
         plos = xpathSApply(b, "//title-group/article-title", xmlValue)[[1]]
  )
}
doi <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//article-id[@pub-id-type='doi']", xmlValue)[[1]],
         plos = xpathSApply(b, "//article-id[@pub-id-type='doi']", xmlValue)[[1]]
  )
}
categories <- function(b, from){
  switch(from, 
         elife = xpathSApply(xpathSApply(b, "//article-categories")[[1]], "//subject", xmlValue),
         plos = xpathSApply(xpathSApply(b, "//article-categories")[[1]], "//subject", xmlValue)
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
         plos = get_auth(b)
  )
}
keywords <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//kwd-group[@kwd-group-type='author-keywords']/kwd", xmlValue),
         plos = NULL
  )
}
body <- function(b, from){
  switch(from, 
         elife = {
           body <- getNodeSet(b, "//body/p")
           body2 <- lapply(body, xmlValue)
           setNames(body2, sapply(body, xmlGetAttr, name="hwp:id"))
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
         plos = xpathSApply(b, "//abstract", xmlValue)
  )
}
exec_summary <- function(b, from){
  switch(from, 
         elife = paste0(xpathSApply(b, "//abstract[@hwp:id='abstract-2']/p", xmlValue), collapse = " "),
         plos = NULL
  )
}

refs_dois <- function(b, from){
  switch(from, 
         elife = xpathSApply(b, "//ref-list/ref//pub-id[@pub-id-type='doi']", xmlValue),
         plos = NULL
  )
}

refs <- function(b, from){
  switch(from, 
         elife = NULL,
         plos = xpathSApply(b, "//ref-list/ref/mixed-citation", xmlValue)
  )
}

publisher <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//publisher")[[1]]),
         plos = xmlToList(xpathSApply(b, "//publisher")[[1]])
  )
}

journal_meta <- function(b, from){
  switch(from, 
         elife = xmlToList(xpathSApply(b, "//journal-meta")[[1]], addAttributes = FALSE),
         plos = xmlToList(xpathSApply(b, "//journal-meta")[[1]], addAttributes = FALSE)
  )
}
