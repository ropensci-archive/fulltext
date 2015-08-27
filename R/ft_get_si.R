#' Download supplementary materials from journals
#' 
#' @param doi DOI of article
#' @param from Source (publisher) of article (optional, but suggested!)
#' @param si.no which supplement (first, second, etc.) as printed in
#' the article, to download. Note that "Fig S1" is not valid; this
#' should be a number
#' @param si.name name of the supplement to be downloaded (see notes)
#' @param save.name a name for the file to download. If \code{NULL}
#' (default) this will be a combination of the DOI and SI number
#' @param dir directory to save file to. If \code{NULL} (default) this
#' will be a temporary directory created for your files
#' @param cache if \code{TRUE} (default), the file won't be downloaded
#' again if it already exists (in a temporary directory creates, or
#' your chosen \code{dir})
#' @author Will Pearse
#' @examples
#' ft_get_si("10.1371/journal.pone.0127900", 1)
#' ft_get_si("10.1371/journal.pone.0127900", 1, "plos")
#' ft_get_si("10.1111/ele.12437", 1)
#' ft_get_si("10.6084/m9.figshare.979288", 1)
#' ft_get_si("E093-059", "myco_db.csv", "esa_archives")
#' ft_get_si("E092-201", "MCDB_communities.csv", "esa_data_archives")
#' ft_get_si("10.1126/science.1255768", "Appendix_BanksLeite_etal.txt")
#' ft_get_si("10.1098/rspb.2015.0338", vol=282, issue=1811, 1)
ft_get_si <- function(doi, si, from=NULL, save.name=NULL, dir=NULL, cache=TRUE, vol=NULL, issue=NULL){
    #Argument handling
    if(!(is.numeric(si) | is.character(si)))
        stop("'si' must be numeric or character")
    if(length(doi) != 1)
        stop("Only one DOI at a time; see help for examples of use'")
    if(!is.null(from))
        from <- match.arg(from, c("plos","wiley","science","proceedings","figshare","esa_data_archives","esa_archives"))
    
    #Setup output directory and filename
    if(!is.null(dir)){
        if(!file.exists(dir))
            stop("'dir' must exist unless NULL")
    } else dir <- tempdir()
    if(is.null(save.name)){
        save.name <- paste(doi,si, sep="_")
        save.name <- gsub(.Platform$file.sep, "_", save.name, fixed=TRUE)
    }

    #Dispatch, download, and return
    if(is.null(from))
        from <- get_si_pub(doi)
    func <- get_si_func(from)
    return(func(doi, si, save.name=save.name, cache=cache, vol=vol, issue=issue))
}

get_si_pub <- function(x){
    if(!is.character(x))
        stop("'x' must be a character")

    #Doing the check here saves one internet call
    if(grepl("figshare", x))
            return("figshare")
    pub <- cr_works(x)$data
    
    if(is.null(pub) || nchar(pub)==0)
        stop("Cannot find publisher for DOI: ", x)
    
    return(.grep.text(pub$member, "[0-9]+"))
}

get_si_func <- function(x) {
    #Check by code, return if found
    output <- switch(x, 
                     "340" = get_si_plos,
                     "311" = get_si_wiley,
                     "221" = get_si_science,
                     "175" = get_si_proceedings
                     )
    if(!is.null(output))
        return(output)

    #Check by letter code
    output <- switch(x, 
                     "plos" = get_si_plos,
                     "wiley" = get_si_wiley,
                     "science" = get_si_science,
                     "proceedings" = get_si_proceedings,
                     "figshare" = get_si_figshare,
                     "esa_data_archives" = get_si_esa_data_archives,
                     "esa_archives" = get_si_esa_archives
                     )
    if(is.null(output))
        stop("Publisher (code) ", x, " not found")
    return(output)
}

get_si_plos <- function(doi, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("PLoS download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    
    #Find journal from DOI
    journals <- setNames(c("plosone", "plosbiology", "plosmedicine", "plosgenetics", "ploscompbiol", "plospathogens", "plosntds"), c("pone", "pbio", "pmed", "pgen", "pcbi", "ppat", "pntd"))
    journal <- gsub("[0-9\\.\\/]*", "", doi)
    journal <- gsub("journal", "", journal)
    if(sum(journal %in% names(journals)) != 1)
        stop("Unrecognised journal in DOI")
    journal <- journals[journal]

    #Download and return
    destination <- file.path(dir, save.name)
    url <- paste0("http://journals.plos.org/", journal, "/article/asset?unique&id=info:doi/", doi, ".s", formatC(si, width=3, flag="0"))
    return(.download(url, dir, save.name, cache))
}

get_si_wiley <- function(doi, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("Wiley download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download SI HTML page and find SI link
    html <- getURL(paste0("http://onlinelibrary.wiley.com/doi/", doi, "/suppinfo"))
    links <- gregexpr("(asset/supinfo/)[-0-9a-zA-Z\\.\\?\\=\\&\\,\\;_]*", as.character(html), useBytes=FALSE)
    pos <- as.numeric(links[[si]])
    link <- substr(html, pos, pos+attr(links[[si]], "match.length")-1)
    url <- paste0("http://onlinelibrary.wiley.com/store/", doi, "/", link)

    #Download and return
    destination <- file.path(dir, save.name)
    return(.download(url, dir, save.name, cache))
}

get_si_figshare <- function(doi, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("FigShare download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    
    #Find, download, and return
    url <- .grep.url(paste0("http://dx.doi.org/", doi), "(http://figshare.com/articles/)[A-Za-z0-9_/]*")
    url <- .grep.url(url, "(http://files\\.figshare\\.com/)[-a-zA-Z0-9\\_/\\.]*", si)
    return(.download(url, dir, save.name, cache))
}

get_si_esa_data_archives <- function(esa, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("ESA Archives download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download, and return
    esa <- gsub("-", "/", esa, fixed=TRUE)
    return(.download(paste0("http://esapubs.org/archive/ecol/", esa, "/data", "/", si), dir, save.name, cache))
}
get_si_esa_archives <- function(esa, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("ESA Archives download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download, and return
    esa <- gsub("-", "/", esa, fixed=TRUE)
    return(.download(paste0("http://esapubs.org/archive/ecol/", esa, "/", si), dir, save.name, cache))
}

get_si_science <- function(doi, si, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("Science download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Find, download, and return
    url <- paste0("http://www.sciencemag.org", .grep.url(paste0("http://www.sciencemag.org/lookup/doi/", doi), "(/content/)[0-9/]*"), "/suppl/DC1")
    url <- paste0("http://www.sciencemag.org", .grep.url(url, "(/content/suppl/)[A-Z0-9/\\.]*(Appendix_BanksLeite_etal.txt)"))
    return(.download(url, dir, save.name, cache))
}

get_si_proceedings <- function(doi, si, vol, issue, save.name=NULL, dir=NULL, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("Proceedings download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    
    #Find, download, and return
    journal <- .grep.text(doi, "(rsp)[a-z]")
    tail <- gsub(".", "", .grep.text(doi, "[0-9]+\\.[0-9]*", 2), fixed=TRUE)
    url <- paste0("http://", journal, ".royalsocietypublishing.org/content/", vol, "/", issue, "/", tail, ".figures-only")
    url <- paste0("http://rspb.royalsocietypublishing.org/", .grep.url(url, "(highwire/filestream)[a-zA-Z0-9_/\\.]*"))
    return(.download(url, dir, save.name))
}

#' Internal regexp functions
.grep.url <- function(url, regexp, which=1){
    html <- getURL(url)
    return(.grep.text(html, regexp, which))
}
.grep.text <- function(text, regexp, which=1){
    links <- gregexpr(regexp, text)
    pos <- as.numeric(links[[1]][which])
    return(substr(text, pos, pos+attr(links[[1]], "match.length")[which]-1))
}

#' Internal download function
.download <- function(url, dir, save.name, cache=TRUE){
    destination <- file.path(dir, save.name)
    if(cache==TRUE & file.exists(destination))
        return(destination)
    result <- download.file(url, destination, quiet=TRUE)
    if(result != 0)
        stop("Error code", result, " downloading file; file may not exist")
    return(destination)
}

.tmpdir <- function(dir){
    if(!is.null(dir)){
        if(!file.exists(dir))
            stop("'dir' must exist unless NULL")
    } else dir <- tempdir()
}
.save.name <- function(doi, save.name, file){
    if(is.null(save.name)){
        save.name <- paste(doi,file, sep="_")
        save.name <- gsub(.Platform$file.sep, "_", save.name, fixed=TRUE)
    }
    return(save.name)
}
