#' Download supplementary materials from journals
#'
#' Put a call to this function where you would put a file-path - everything 
#' is cached by default, so you don't have to worry about multiple downloads 
#' in the same session.
#' 
#' @details The examples probably give the best indication of how to
#' use this function. In general, just specify the DOI of the article
#' you want to download data from, and the number of the supplement
#' you want to download (1, 5, etc.). ESA journals don't use DOIs
#' (give the article code; see below), and Proceedings, Science, and
#' ESA journals need you to give the filename of the supplement to
#' download.
#'
#' Below is a list of all the publishers this supports, and examples
#' of articles from them. I'm aware that there isn't perfect overlap
#' between these publishers and the rest of the package; I plan to
#' correct this in the near future.
#'
#' \describe{
#' \item{auto}{Default. Use a cross-ref search
#' (\code{\link[rcrossref:cr_works]{cr_works}}) on the DOI to
#' determine the publisher.}
#' \item{plos}{Public Library of Science journals (e.g., PLoS One;
#' \url{http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0126524})}
#' \item{wiley}{Wiley journals, (e.g.,
#' \url{http://onlinelibrary.wiley.com/doi/10.1111/ele.12289/abstract}}
#' \item{science}{Science magazine (e.g.,
#' \url{http://www.sciencemag.org/content/345/6200/1041.short})}
#' \item{proceedings}{Royal Society of London journals (e.g.,
#' \url{http://rspb.royalsocietypublishing.org/content/282/1814/20151215}). Requires
#' \code{vol} and \code{issue} of the article.}
#' \item{figshare}{Figshare, (e.g., \url{http://bit.ly/figshare-example})}
#' \item{\code{esa_data_archives} & \code{esa_data}}{You must give
#' article codes, not DOIs, for these, which you can find on the
#' article itself. An ESA Data Archive paper - not to be confused with
#' an ESA Archive, which is the supplement to an ESA paper. The
#' distinction seems less crazy once you're reading the paper - if it
#' only describes a dataset, it's an \code{esa_archive} paper, else
#' it's an \code{esa_data_archive}. For example,
#' \url{http://www.esapubs.org/archive/ecol/E092/201/default.htm} is
#' an \code{esa_data_archive} whose article code is E092-201-D1;
#' \url{http://esapubs.org/Archive/ecol/E093/059/default.htm} is a
#' \code{esa_archive} whose code is E093-059-D1.}
#' }
#' @param doi DOI of article (\code{character}). Note: if using ESA
#' journal, this must be the ESA-specific article code (e.g.,
#' E092-201).
#' @param si number of the supplement to be downloaded (1, 2, 3, etc.),
#' or (for ESA and Science journals) the name of the supplment (e.g.,
#' "S1_data.csv"). Can be a \code{character} or \code{numeric}.
#' @param from Publisher of article (\code{character}). Optional,
#' except for ESA journals (see \code{doi}), but supplying it will
#' speed up downloads. Must be one of: auto (i.e., auto-detect journal;
#' default), plos, wiley, science, proceedings, figshare,
#' esa_data_archives, esa_archives.
#' @param save.name a name for the file to download
#' (\code{character}). If \code{NULL} (default) this will be a
#' combination of the DOI and SI number
#' @param dir directory to save file to (\code{character}). If
#' \code{NULL} (default) this will be a temporary directory created
#' for your files
#' @param cache if \code{TRUE} (default), the file won't be downloaded
#' again if it already exists (in a temporary directory creates, or
#' your chosen \code{dir})
#' @param vol Article volume (Proceedings journals only;
#' \code{numeric})
#' @param issue Article issue (Proceedings journals only;
#' \code{numeric})
#' @author Will Pearse (\email{will.pearse@@gmail.com})
#' @examples
#' \dontrun{
#' #Put the function wherever you would put a file path
#' crabs <- read.csv(ft_get_si("10.6084/m9.figshare.979288", 2))
#'
#' #ESA data papers and regular articles *must* be marked
#' fungi <- read.csv(ft_get_si("E093-059", "myco_db.csv",
#'                                         "esa_archives"))
#' mammals <- read.csv(ft_get_si("E092-201", "MCDB_communities.csv",
#'                                             "esa_data_archives"))
#' }
#' @export
ft_get_si <- function(doi, si, from=c("auto","plos","wiley","science","proceedings","figshare","esa_data_archives","esa_archives"), save.name=NULL, dir=NULL, cache=TRUE, vol=NULL, issue=NULL){
    #Argument handling
    if(!(is.numeric(si) | is.character(si)))
        stop("'si' must be numeric or character")
    if(length(doi) != 1)
        stop("Only one DOI at a time; see help for examples of use'")
    from <- match.arg(from)
    
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
    if(from == "auto")
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
    
    if(is.null(pub) || nchar(pub)==0 || pub$prefix=="http://id.crossref.org/prefix/10.0000")
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
    html <- as.character(GET(paste0("http://onlinelibrary.wiley.com/doi/", doi, "/suppinfo")))
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
    url <- .grep.url(paste0("http://dx.doi.org/", doi), "(http://files\\.figshare\\.com/)[-a-zA-Z0-9\\_/\\.]*", si)
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

# Internal regexp functions
.grep.url <- function(url, regexp, which=1){
    html <- as.character(GET(url))
    return(.grep.text(html, regexp, which))
}
.grep.text <- function(text, regexp, which=1){
    links <- gregexpr(regexp, text)
    pos <- as.numeric(links[[1]][which])
    return(substr(text, pos, pos+attr(links[[1]], "match.length")[which]-1))
}

# Internal download function
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
