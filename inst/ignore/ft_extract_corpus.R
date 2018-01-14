#' Extract text from one to many pdf documents into a tm corpus
#'
#' @export
#' @param paths Path to one or more pdfs, and they must exist
#' @return A tm Corpus (or VCorpus, later that is)
#' @seealso [ft_extract()]
#' @examples \dontrun{
#' path <- system.file("examples", "example1.pdf", package = "fulltext")
#' (res <- ft_extract_corpus(path))
#' tm::TermDocumentMatrix(res$data)
#' }
ft_extract_corpus <- function(paths) {
  paths <- process_paths(paths)
  txts <- lapply(paths, crminer::crm_extract)
  out <- tm::Corpus(tm::VectorSource(pluck(txts, "text")))
  meta <- get_meta(out)
  structure(list(meta = meta, data = out), class = "ft_extract")
}

files_exist <- function(x){
  tmp <- sapply(x, file.exists)
  if (!all(tmp)) stop(sprintf("These do not exist or can not be found:\n%s",
                             paste(names(tmp[tmp == FALSE]), collapse = "\n") ))
}

get_meta <- function(input){
  do.call(rbind_fill, lapply(input, function(x) {
    tmp <- attributes(x)
    tmp[sapply(tmp, length) == 0] <- NA
    tmp <- lapply(tmp, function(z) if (length(z) > 1) 
      paste(z, collapse = ", ") else z)
    data.frame(tmp, stringsAsFactors = FALSE)
  }))
}

process_paths <- function(x){
  files_exist(x)
  path.expand(x)
}
