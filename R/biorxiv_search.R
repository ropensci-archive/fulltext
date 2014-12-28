#'Search biorxiv.org
#' @description return full text objects from searches of biorxiv.org
#' @param query the terms to search for in biorxiv
#' @param limit the number of results to return
#' @example \dontrun{
#'   ### Get url's for a search term
#'   urls <- biorxiv_search("ecology",limit=20) 
#' }
#' @return a list with the following elements: a vector of URL's for bioRxiv papers from the search terms,and the maximum number of results
#' @export
#' @import XML tm

biorxiv_search <- function(query, limit = 10){
  base <- "http://www.biorxiv.org/search/"
  page <- "?page="
  URL <- paste(base,URLencode(query),page,"0",sep="") 
  pgRes <- htmlParse(URL)
  
  ## Get total number of results possible
  ## Be careful, this may be a fragile way to extract the information, because it just relies on H1 from the header
  maxRes <- xpathApply(pgRes,"//h1",xmlValue)
  maxRes <- as.numeric(regmatches(maxRes[[1]],regexpr('?[0-9]+' ,maxRes[[1]])))
  
  ## Handle a situation with no results
  if(identical(maxRes, numeric(0))){
    return(list(data=NULL,found=0))
  }
  ## Get the total number of pages
  pgCtLst  <-  xpathApply(pgRes, "//li/a")
  lastIndex <- which(unlist(lapply(pgCtLst,function(x){grepl("Last",xmlValue(x))}))==TRUE)
  ## Handle if there is just 1 page of results
  if(identical(lastIndex, integer(0))){
    lastPg <- 1
  } else {
    lastPg <- as.numeric(strsplit(xmlGetAttr(pgCtLst[[lastIndex]],"href"),"=")[[1]][2])
  }
  ## Get first round of URL's 
  ftURL <- unlist(lapply(xpathApply(pgRes, "//a[@class]",xmlGetAttr, "href"),grep,pattern="http://www.biorxiv.org/content/early",value=T))
  
  ### Adjust for limits
  
  if(limit < maxRes){
    lastPg <- (limit%%10)+1
  }
    
  ### Loop through all the pages if more than one.
  
  if(lastPg > 0){
    for(i in 1:lastPg){
      URL <- paste(base,URLencode(query),page,i,sep="") 
      pgRes <- htmlParse(URL)
      
    tmpURL <- unlist(lapply(xpathApply(pgRes, "//a[@class]",xmlGetAttr, "href"),grep,pattern="http://www.biorxiv.org/content/early",value=T))
    ftURL <- c(ftURL,tmpURL)
    }
  } 
  if(limit < length(ftURL)){
    ftURL <- ftURL[1:limit]
  }
  ftURL <- as.matrix(ftURL)
  colnames(ftURL) <- "URL"
  bioX <- list(data = ftURL, found = maxRes)
  return(bioX)
  
}

