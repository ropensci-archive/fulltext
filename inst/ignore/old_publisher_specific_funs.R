# individual publiser methods
# pub_mdpi <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     lks <- list()
#     for (i in seq_along(pat$urls)) {
#       lks[[i]] <- data.frame(
#         url = sprintf(pat$urls[[i]], issn, res$volume, res$issue,
#           strextract(doi, pat$components$doi$regex)),
#         content_type = get_ctype(names(pat$urls)[i])
#       )
#     }
#     lks <- do.call(rbind, lks)
#     for (i in seq_len(NROW(lks))) {
#       url_update(doi, lks[i,]$url, lks[i,]$content_type %||% "")
#     }
#   }
#   to_df(doi, pat, member, issn, lks)
# }

# pub_pnas <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     lks <- list()
#     for (i in seq_along(pat$urls)) {
#       lks[[i]] <- data.frame(
#         url = sprintf(pat$urls[[i]], res$volume, res$issue,
#           sub("E", "", strsplit(res$page, "-")[[1]][1])),
#         content_type = get_ctype(names(pat$urls)[i])
#       )
#     }
#     lks <- do.call(rbind, lks)
#     for (i in seq_len(NROW(lks))) {
#       url_update(doi, lks[i,]$url, lks[i,]$content_type %||% "")
#     }
#   }
#   to_df(doi, pat, member, issn, lks)
# }
# pub_hindawi <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     links <- res$link[[1]]
#     use <- links[links$content.type == "application/pdf",]
#     lks <- data.frame(url = use$URL, content_type = use$content.type)
#   }
#   url_update(doi, lks$url, lks$content_type %||% "")
#   to_df(doi, pat, member, issn, lks)
# }
# pub_aaas <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     lks <- list(url = fat_cat_link(doi), 'content-type' = "pdf")
#     if (is.na(lks$url)) {
#       z <- Filter(function(x) grepl(paste0(unlist(x$issn), collapse="|"), issn), pat$journals)[[1]]
#       if (grepl("2375-2548", issn)) {
#         last_part = strextract(doi, z$components$doi$regex, perl=TRUE)
#       } else {
#         last_part = strsplit(res$page, "-")[[1]][1]
#       }
#       url = sprintf(z$urls$pdf, res$volume, res$issue, last_part)
#       lks <- list(url = url, 'content-type' = "pdf")
#     }
#   }
#   url_update(doi, lks$url, lks$content_type %||% "")
#   to_df(doi, pat, member, issn, lks)
# }
# pub_cdc <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     issn <- res$issn
#     year <- strsplit(res$created, "-")[[1]][1]
#     doi_part <- last(strsplit(last(strsplit(doi, '/')[[1]]), '\\.')[[1]])
#     last_part <- paste(substring(doi_part, 1, 2),
#       substring(doi_part, 3, nchar(doi_part)), sep="_")
#     url <- sprintf(pat$urls$pdf, year, last_part)
#     lks <- data.frame(url = url, content_type = "pdf")
#   }
#   url_update(doi, lks$url, lks$content_type %||% "")
#   to_df(doi, pat, member, issn, lks)
# }
# pub_elsevier <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     issn <- res$issn
#     lks <- make_links_no_regex(res$link[[1]]$URL, res$link[[1]]$content.type)
#     for (i in seq_along(lks)) {
#       url_update(doi, lks[[i]]$url, lks[[i]]$`content-type` %||% "")
#     }
#     # FIXME: see alternative-id bit in pubpatternsapi
#   }
#   to_df(doi, pat, member, issn, lks)
# }
# pub_american_society_for_microbiology <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     issn <- res$issn
#     url <- pat$urls$pdf
#     vol <- res$volume
#     iss <- res$issue
#     journal_bit <- tolower(strextract(doi, "[A-Za-z]+"))
#     other_bit <- strextract(strsplit(doi, "/")[[1]][2], "[0-9]+-[0-9]+")
#     lk <- sprintf(url, journal_bit, vol, iss, other_bit)
#     lks <- list(url = lk, 'content-type' = "pdf")
#     url_update(doi, lks$url, lks$`content-type`)
#   }
#   to_df(doi, pat, member, issn, lks)
# }
# pub_de_gruyter <- function(doi, pat, member, issn, res=NULL) {
#   if (all("link" == names(res))) {
#     lks <- links2df(res$link[[1]])
#   } else {
#     url <- res$link[[1]][res$link[[1]]$intended.application == "similarity-checking", ]$URL
#     if (!grepl("view", url)) {
#       url <- sub("\\.xml", "\\.pdf", url)
#     } else {
#       base <- strextract(dirname(url), "https?://[A-Za-z.]+")
#       base <- file.path(base, "downloadpdf/journals")
#       vol <- res$volume
#       iss <- res$issue
#       jabbrev <- strextract(last(strsplit(doi, "/")[[1]]), "[A-Za-z]+")
#       page <- res$page
#       if (is.null(page)) {
#         url <- NULL
#       } else {
#         last_part <- sprintf("article-p%s.pdf", page)
#         url <- file.path(base, jabbrev, vol, iss, last_part)
#       }
#     }
#     lks <- data.frame(url = url, content_type = "pdf")
#     url_update(doi, lks$url, lks$content_type)
#   }
#   to_df(doi, pat, member, issn, lks)
# }
