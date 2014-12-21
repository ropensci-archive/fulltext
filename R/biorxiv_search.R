#'Search biorxiv.org
#' @description return full text objects from searches of biorxiv.org
#' @param query the terms to search for in biorxiv
#' @param limit the number of results to return
#' @example \dontrun{
#'   ### Get url's for a search term
#'   urls <- biorxiv_search("ecology",limit=20) 
#' }
#' @return a vector of URL's for bioRxiv papers from the search terms
#' @export
#' @import XML tm

biorxiv_search <- function(query, limit = 10){
  base <- "http://www.biorxiv.org/search/"
  page <- "?page="
  URL <- paste(base,URLencode(query),page,"0",sep="") 
  pgRes <- htmlParse(URL)
  
  ## Get the total number of results possible
  pgCtLst  <-  xpathApply(pgRes, "//li/a")
  lastIndex <- which(unlist(lapply(pgCtLst,function(x){grepl("Last",xmlValue(x))}))==TRUE)
  lastPg <- as.numeric(strsplit(xmlGetAttr(pgCtLst[[lastIndex]],"href"),"=")[[1]][2])
  
  ## Get first round of URL's 
  ftURL <- unlist(lapply(xpathApply(pgRes, "//a[@class]",xmlGetAttr, "href"),grep,pattern="http://www.biorxiv.org/content/early",value=T))
  
  ### Adjust for limits
  resCt <- 10*lastIndex
  if(limit < resCt){
    lastIndex <- (resCt%%limit)+1
  }
    
  ### Loop through all the pages if more than one.
  
  if(lastPg > 0){
    for(i in 1:lastIndex){
      URL <- paste(base,URLencode(query),page,i,sep="") 
      pgRes <- htmlParse(URL)
      
    tmpURL <- unlist(lapply(xpathApply(pgRes, "//a[@class]",xmlGetAttr, "href"),grep,pattern="http://www.biorxiv.org/content/early",value=T))
    ftURL <- c(ftURL,tmpURL)
    }
  } 
  if(limit < length(ftURL)){
    ftURL <- ftURL[1:limit]
  }
  return(ftURL)
  
}

