

# Internal regexp functions
.grep.url <- function(url, regexp, which=1){
    html <- as.character(GET(url))
    return(.grep.text(html, regexp, which))
}
.grep.text <- function(text, regexp, which=1){
    links <- gregexpr(regexp, text)
    if(which > length(links[[1]]))
        stop("SI number '", which, "' greater than number of detected SIs (", length(links[[1]]), ")")
    pos <- as.numeric(links[[1]][which])
    return(substr(text, pos, pos+attr(links[[1]], "match.length")[which]-1))
}
.file.suffix <- function(text, max.length=4){
    suffix <- .grep.text(text, "[a-zA-Z]+$")
    if(nchar(suffix) <= max.length & nchar(suffix) > 0)
        return(suffix)
    return(NA)
}

# Internal download function
.download <- function(url, dir, save.name, cache=TRUE, ...){
    destination <- file.path(dir, save.name)
    suffix <- .file.suffix(url, 4)
    
    if (cache && file.exists(destination)) {
      if (!is.na(suffix)) {
        attr(destination, "suffix") <- suffix
      }
      return(destination)
    }
    
    result <- GET(url, write_disk(destination, TRUE), ...)
    if (result$status_code > 201) {
      stop(result$status_code, ": downloading file; file may not exist", call. = FALSE)
    }
    
    if (!is.na(suffix)) {
      attr(destination, "suffix") <- suffix
    }
    return(destination)
}

# Internal unzip function
.unzip <- function(zip, dir, save.name, cache, si, list=FALSE){
    files <- utils::unzip(zip, list=TRUE)
    if(list){
        message("Files in ZIP:")
        print(files)
    }
    if(!si %in% files$Name)
        stop("Required file not in zipfile ", zip)
    file <- utils::unzip(zip, si)
    file.rename(file, file.path(dir, save.name))
    return(file.path(dir, save.name))
}

# Internal URL 'redirect' function
.url.redir <- function(x)
    return(GET(x)$url)

.tmpdir <- function(dir){
    if(!is.na(dir)){
        if(!file.exists(dir))
            stop("'dir' must exist unless NA")
    } else dir <- tempdir()
    return(dir)
}
.save.name <- function(doi, save.name, file){
    if(is.na(save.name)){
        save.name <- paste(doi,file, sep="_")
        save.name <- gsub(.Platform$file.sep, "_", save.name, fixed=TRUE)
    }
    return(save.name)
}
#Expanding arguments when handling ft-classes
.fix.param <- function(x, param, name){
    if(length(x) != length(param)){
        if((length(x) %% length(param)) != 0)
            stop("length of ", "name (", length(param), ") is incompatible with 'x' (", length(x), ")")
        param <- rep(param, length(x))
    }
    return(param)
}
