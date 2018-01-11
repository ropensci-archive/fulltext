#' fulltext warnings details
#' 
#' What can you do about the various warnings?
#' 
#' This document is in relation to the function [ft_get()]
#' 
#' @section No plugin:
#' 
#' For the warning "no plugin for Crossref ...", this is 
#' what happened internally:
#' 
#' This happens when we don't have a hard coded
#' plugin for that specific publisher within this packge
#' (use `ft_get_ls()` to see what hard coded publisher plugins
#' we have), but we do have generic functions for Crossref and 
#' ftdoi.org that are also tried and may get a result. You
#' are welcome to open up an issue at 
#' <https://github.com/ropensci/fulltext/issues> to discuss 
#' publisher specific plugins.
#' 
#' 
#' @section Access or an error:
#' 
#' For the warning "you may not have access to ... or an error occurred"
#' we've likely tried to get the full text but either an error 
#' occurred (which can be a lot of things), or you don't have access
#' to the full text.
#' 
#' If you think the problem may be that you don't have access, 
#' check whether you are on an IP address that has access to the 
#' full text, and if you're not, get on one that does - most 
#' likely by being on campus/etc. or through a VPN.
#' 
#' 
#' @section Part of an article:
#' 
#' For the warning "... was not found or may be a DOI for a part of an article"
#' this happens for certain publishers (e.g., PLOS) that issue DOIs for 
#' parts of articles (e.g., abstract, body, supplements) - in which case it 
#' doesn't make sense to get full text of, for example, supplements.
#' 
#' 
#' @section No Crossref link:
#' 
#' For the warning "no link found from Crossref", this happens when 
#' we've gone through the route of searching for a full text URL from
#' the Crossref API, and there wasn't one, so we stop searching and 
#' give that warning.
#' 
#' 
#' @name ft_get-warnings
NULL
