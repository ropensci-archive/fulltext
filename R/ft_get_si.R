#' Download supplementary materials from journals
#'
#' Put a call to this function where you would put a file-path - everything 
#' is cached by default, so you don't have to worry about multiple downloads 
#' in the same session.
#' 
#' @param x One of: vector of DOI(s) of article(s) (a
#' \code{character}), output from \code{\link{ft_get}}, or output from
#' \code{\link{ft_search}}. Note: if using ESA journal, you can *only*
#' use the ESA-specific article code (e.g., E092-201).
#' @param si number of the supplement to be downloaded (1, 2, 3, etc.),
#' or (for ESA and Science journals) the name of the supplment (e.g.,
#' "S1_data.csv"). Can be a \code{character} or \code{numeric}.
#' @param from Publisher of article (\code{character}). The default
#' (\code{auto}) uses crossref (\code{\link[rcrossref]{cr_works}}) to
#' detect the journal's publisher. Specifying the journal can somewhat
#' speed up your download, or be used to force a download from EPMC
#' (see details). You *must* specify if downloading from an ESA
#' journal (\code{esa_data_archives}, \code{esa_archives}). You can
#' only use this argument if \code{x} is a vector of DOI(s). Must be
#' one of: \code{auto} (i.e., auto-detect journal; default),
#' \code{plos}, \code{wiley}, \code{science}, \code{proceedings},
#' \code{figshare}, \code{esa_data_archives}, \code{esa_archives},
#' \code{biorxiv}, or \code{epmc}.
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
#' @param list if \code{TRUE}, print all files within a zip-file
#' downloaded from EPMC (default: FALSE). This is *very* useful if
#' using EPMC (see notes)
#' @param timeout how long to wait for successful download (default 10
#' seconds)
#' @author Will Pearse (\email{will.pearse@@gmail.com})
#' @note Make sure that the article from which you're attempting to
#' download supplementary materials *has* supplementary materials. 404
#' errors and 'file not found' errors can result from such cases.
#' @examples
#' \dontrun{
#' #Put the function wherever you would put a file path
#' crabs <- read.csv(ft_get_si("10.6084/m9.figshare.979288", 2))
#'
#' #View the suffix (file extension) of downloaded files
#' # - note that not all files are uploaded/stored with useful file extensions!
#' ft_get_si("10.6084/m9.figshare.979288", 2)
#' attr(ft_get_si("10.6084/m9.figshare.979288", 2), "suffix")
#'
#' #ESA data papers and regular articles *must* be marked
#' fungi <- read.csv(ft_get_si("E093-059", "myco_db.csv",
#'                                         "esa_archives"))
#' mammals <- read.csv(ft_get_si("E092-201", "MCDB_communities.csv",
#'                                             "esa_data_archives"))
#' epmc.fig <- ft_get_si("10.1371/journal.pone.0126524", "pone.0126524.g005.jpg", "epmc")
#' #...note this 'SI' is not actually an SI, but rather an image from the paper.
#' }
#' @template ft_get_si
#' @export
ft_get_si <- function(x, si, from=c("auto","plos","wiley","science","proceedings","figshare","esa_data_archives","esa_archives","biorxiv","epmc"), save.name=NA, dir=NA, cache=TRUE, vol=NA, issue=NA, list=FALSE, timeout=10) UseMethod("ft_get_si")
#' @export
#' @rdname ft_get_si
ft_get_si.character <- function(x, si, from=c("auto","plos","wiley","science","proceedings","figshare","esa_data_archives","esa_archives","biorxiv","epmc"), save.name=NA, dir=NA, cache=TRUE, vol=NA, issue=NA, list=FALSE, timeout=10){
    #Basic argument handling
    if(length(x) == 0)
        stop("'x' must contain some data!")
    from <- match.arg(from)
    if(!(is.numeric(si) | is.character(si)))
        stop("'si' must be numeric or character")

    #Multiply argument lengths
    from <- .fix.param(x, from, "from")
    si <- .fix.param(x, si, "si")
    save.name <- .fix.param(x, save.name, "save.name")
    dir <- .fix.param(x, dir, "dir")
    vol <- .fix.param(x, vol, "vol")
    issue <- .fix.param(x, issue, "issue")
    cache <- .fix.param(x, cache, "cache")
    list <- .fix.param(x, list, "list")
    timeout <- .fix.param(x, timeout, "timeout")
    
    ############################
    #Recurse if needed (can't use Recall because of potential argument length problems)
    if(length(x) > 1)
        return(setNames(unlist(mapply(ft_get_si.character, x=x,si=si,from=from,save.name=save.name,dir=dir,cache=cache,vol=vol,issue=issue,list=list,timeout=timeout)),x))
    ############################
    #...Do work

    #Setup output directory and filename
    if(!is.na(dir)){
        if(!file.exists(dir))
            stop("'dir' must exist unless NA")
    } else dir <- tempdir()
    if(is.na(save.name)){
        save.name <- paste(x,si, sep="_")
        save.name <- gsub(.Platform$file.sep, "_", save.name, fixed=TRUE)
    }

    #Find publisher, download, and return
    if(from == "auto")
        from <- get_si_pub(x)
    func <- get_si_func(from)
    return(func(x, si, save.name=save.name, cache=cache, vol=vol, issue=issue, list=list, timeout=timeout))
}
#' @export
#' @rdname ft_get_si
ft_get_si.ft_data <- function(x, si, from=NA, save.name=NA, dir=NA, cache=TRUE, vol=NA, issue=NA, list=FALSE, timeout=10){
    if(!is.na(from))
        stop("Cannot use 'from' argument with 'ft_data' input")
    from <- names(x)
    x <- unlist(sapply(x, function(x) x$dois))
    from <- .fix.param(x, from, "from")
    return(setNames(unlist(mapply(ft_get_si.character, x=x,si=si,from=from,save.name=save.name,dir=dir,cache=cache,vol=vol,issue=issue,list=list,timeout=timeout)),x))
}
#' @export
#' @rdname ft_get_si
ft_get_si.ft <- function(x, si, from=NA, save.name=NA, dir=NA, cache=TRUE, vol=NA, issue=NA, list=FALSE, timeout=10){
    if(!is.na(from))
        stop("Cannot use 'from' argument with 'ft' input")
    x <- unlist(sapply(x, function(x) x$data$id))
    from <- names(x)
    return(setNames(unlist(mapply(ft_get_si.character, x=x,si=si,from=from,save.name=save.name,dir=dir,cache=cache,vol=vol,issue=issue,list=list,timeout=timeout)),x))
}
