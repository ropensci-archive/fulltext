get_si_pub <- function(x){
    if(!is.character(x))
        stop("'x' must be a character")

    #Doing the check here saves one internet call
    if(grepl("figshare", x))
            return("figshare")
    pub <- cr_works(x)$data
    
    if(pub$prefix=="http://id.crossref.org/prefix/10.0000")
        stop("Cannot find publisher for DOI: ", x)
    
    return(.grep.text(pub$member, "[0-9]+"))
}

get_si_func <- function(x) {
    #Check by code, return if found
    output <- switch(x, 
                     "340" = get_si_plos,
                     "311" = get_si_wiley,
                     "221" = get_si_science,
                     "175" = get_si_proceedings,
                     "246" = get_si_biorxiv
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
                     "esa_archives" = get_si_esa_archives,
                     "biorxiv" = get_si_biorxiv,
                     "epmc" = get_si_epmc
                     )
    #If all else fails, try EPMC
    if(is.null(output))
        output <- get_si_epmc
    return(output)
}

get_si_plos <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, ...){
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
        stop("Unrecognised PLoS journal in DOI ", doi)
    journal <- journals[journal]

#Download and return
    destination <- file.path(dir, save.name)
    url <- paste0("http://journals.plos.org/", journal, "/article/asset?unique&id=info:doi/", doi, ".s", formatC(si, width=3, flag="0"))
    return(.download(url, dir, save.name, cache))
}

#' @importFrom httr timeout
#' @importFrom xml2 read_html xml_attr xml_find_all
get_si_wiley <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, timeout=10, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("Wiley download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download SI HTML page and find SI link
    # - requires check for new Ecology Letters page (...the page seems buggy...)
    html <- tryCatch(as.character(GET(paste0("http://onlinelibrary.wiley.com/doi/", doi, "/full"), httr::timeout(timeout))),
                     silent=TRUE, error = function(x) NA)
    if(is.na(html))
        html <- as.character(GET(paste0("http://onlinelibrary.wiley.com/wol1/doi/", doi, "/full"), httr::timeout(timeout)))
    links <- gregexpr("(asset/supinfo/)[-0-9a-zA-Z\\.\\?\\=\\&\\,\\;_]*", html, useBytes=FALSE)
    if(any(links[[1]] == -1))
        links <- gregexpr("(asset/supinfo)[-0-9a-zA-Z\\.\\?\\=\\&\\,\\;_%]*", html, useBytes=FALSE)
    
    html <- read_html(html)
    urls <- xml_attr(xml_find_all(html, '//a[contains(@href,"supinfo")]'), "href")
    if(si > length(urls))
        stop("SI number '", si, "' greater than number of detected SIs (", length(urls), ")")
    url <- urls[si]
    
    #Download and return
    destination <- file.path(dir, save.name)
    return(.download(url, dir, save.name, cache))
}

get_si_figshare <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("FigShare download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    
    #Find, download, and return
    url <- .grep.url(paste0("http://dx.doi.org/", doi), "(http://files\\.figshare\\.com/)[-a-zA-Z0-9\\_/\\.]*", si)
    return(.download(url, dir, save.name, cache))
}

get_si_esa_data_archives <- function(esa, si, save.name=NA, dir=NA, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("ESA Archives download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download, and return
    esa <- gsub("-", "/", esa, fixed=TRUE)
    return(.download(paste0("http://esapubs.org/archive/ecol/", esa, "/data", "/", si), dir, save.name, cache))
}
get_si_esa_archives <- function(esa, si, save.name=NA, dir=NA, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("ESA Archives download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Download, and return
    esa <- gsub("-", "/", esa, fixed=TRUE)
    return(.download(paste0("http://esapubs.org/archive/ecol/", esa, "/", si), dir, save.name, cache))
}

get_si_science <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, ...){
    #Argument handling
    if(!is.character(si))
        stop("Science download requires character SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)

    #Find, download, and return
    url <- paste0("http://www.sciencemag.org", .grep.url(paste0("http://www.sciencemag.org/lookup/doi/", doi), "(/content/)[0-9/]*"), "/suppl/DC1")
    url <- paste0("http://www.sciencemag.org", .grep.url(url, "(/content/suppl/)[A-Z0-9/\\.]*"))
    return(.download(url, dir, save.name, cache))
}

get_si_proceedings <- function(doi, si, vol, issue, save.name=NA, dir=NA, cache=TRUE, ...){
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

#' @importFrom xml2 xml_text xml_find_one read_xml
get_si_epmc <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, list=FALSE, ...){
    #Argument handling
    if(!is.character(si))
        stop("EPMB download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    zip.save.name <- .save.name(doi, NA, "raw_zip.zip")
    
    #Find, download, and return
    pmc.id <- xml_text(xml_find_one(read_xml(paste0("http://www.ebi.ac.uk/europepmc/webservices/rest/search/query=", doi)), ".//pmcid"))
    url <- paste0("http://www.ebi.ac.uk/europepmc/webservices/rest/", pmc.id[[1]], "/supplementaryFiles")
    zip <- tryCatch(.download(url,dir,zip.save.name,cache), error=function(x) stop("Cannot find supplementary materials for (seemingly) valid EPMC article ID ",pmc.id[[1]]))
    return(.unzip(zip, dir, save.name, cache, si, list))
}

get_si_biorxiv <- function(doi, si, save.name=NA, dir=NA, cache=TRUE, ...){
    #Argument handling
    if(!is.numeric(si))
        stop("bioRxiv download requires numeric SI info")
    dir <- .tmpdir(dir)
    save.name <- .save.name(doi, save.name, si)
    
    #Find, download, and return
    url <- paste0(.url.redir(paste0("http://dx.doi.org/", doi)), ".figures-only")
    file <- .grep.url(url, "/highwire/filestream/[a-z0-9A-Z\\./_-]*", si)
    return(.download(.url.redir(paste0("http://biorxiv.org",file)), dir, save.name, cache))
}
