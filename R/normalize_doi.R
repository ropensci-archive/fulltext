# normalize DOI
## id: a single DOI
doi_normalize <- function(id) {
  id_norm <- gsub("/|\\.|-|:|;|\\(|\\)|<|>", "_", id)
  doi_store(id, id_norm)
  return(id_norm)
}

# store a DOI
## ids: a single DOI
## id_norm: a single normalized DOI
doi_store <- function(id, id_norm) {
  fulltext_store$set(id_norm, id)
}
## ids: a vector of DOIs
## id_norm: a vector of normalized DOIs
dois_store <- function(ids, id_norm) {
  fulltext_store$mset(id_norm, ids)
}

# lookup a DOI
## id_norm: a normalized DOI
doi_lookup <- function(id_norm) {
  fulltext_store$get(id_norm)
}
## ids_norm: a vector of normalized DOIs
## - returns NA's for items not found, so function shouldn't fail
dois_lookup <- function(ids_norm) {
  data.frame(
    dois = unlist(fulltext_store$mget(ids_norm, missing = NA_character_)), 
    ids_norm = ids_norm, 
    stringsAsFactors = FALSE
  )
}
