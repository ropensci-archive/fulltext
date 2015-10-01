# plugin_get_crossref <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("entrez", sources))) {
#     opts$ids <- ids
#     out <- do.call(entrez_get, opts)
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = NULL, data = out, opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_entrez <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("entrez", sources))) {
#     opts$ids <- ids
#     out <- as.list(do.call(entrez_get, opts))
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL,  data = NULL, opts = opts)
#   }
# }

# plugin_get_plos <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("plos", sources))) {
#     # remove annotations
#     ids <- grep("annotation", ids, value = TRUE, invert = TRUE)
#     check_dois(ids)
#     opts$doi <- ids
#     opts$callopts <- callopts
#     out <- do.call(plos_fulltext, opts)
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_bmc <- function(sources, query, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("bmc", sources))) {
#     opts$uris <- query
#     opts$raw <- TRUE
#     out <- do.call(bmc_xml, opts)
#     dois <- sapply(out, function(x) {
#       xml2::xml_text(xml2::xml_find_one(xml2::read_xml(x), "//fm//bibl//pubid[@idtype='doi']"))
#     })
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = dois, data = out, opts = opts)
#   } else {
#     list(found = NULL, dois = NULL,  data = NULL, opts = opts)
#   }
# }

# plugin_get_bmc_dois <- function(sources, dois, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("bmc", sources))) {
#     opts$dois <- dois
#     opts$callopts <- callopts
#     out <- do.call(bmc_ft, opts)
#     names(out) <- dois
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = dois, data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL,  data = NULL, opts = opts)
#   }
# }

# plugin_get_elife <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("elife", sources))) {
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(elife_paper, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_peerj <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("peerj", sources))) {
#     check_dois(ids)
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(peerj_ft, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_frontiersin <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("frontiersin", sources))) {
#     check_dois(ids)
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(frontiersin_ft, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_pensoft <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("pensoft", sources))) {
#     check_dois(ids)
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(pensoft_ft, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_copernicus <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("copernicus", sources))) {
#     check_dois(ids)
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(copernicus_ft, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_cogent <- function(sources, ids, opts, path = NULL, ...){
#   callopts <- list(...)
#   if (any(grepl("cogent", sources))) {
#     check_dois(ids)
#     opts$dois <- ids
#     opts$callopts <- callopts
#     out <- do.call(cogent_ft, opts)
#     names(out) <- ids
#     attr(out, "format") <- "xml"
#     list(found = length(out), dois = names(out), data = construct_paths(cache_options_get(), out), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_arxiv <- function(sources, ids, opts, path, ...){
#   callopts <- list(...)
#   if (any(grepl("arxiv", sources))) {
#     opts$dois <- ids
#     opts$callopts <- callopts
#     opts$basepath <- path
#     out <- do.call(arxiv_ft, opts)
#     names(out) <- ids
#     list(found = length(out), dois = names(out), data = list(backend = NULL,
#                                                              path = out,
#                                                              data = NULL), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }

# plugin_get_biorxiv <- function(sources, ids, opts, path, ...){
#   callopts <- list(...)
#   if (any(grepl("biorxiv", sources))) {
#     opts <- list()
#     opts$dois <- ids
#     opts$callopts <- callopts
#     opts$basepath <- path
#     out <- do.call(biorxiv_ft, opts)
#     names(out) <- ids
#     list(found = length(out), dois = names(out), data = list(backend = NULL,
#                                                              path = out,
#                                                              data = NULL), opts = opts)
#   } else {
#     list(found = NULL, dois = NULL, data = NULL, opts = opts)
#   }
# }
