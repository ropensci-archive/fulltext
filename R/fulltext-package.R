#' @title Fulltext search and retrieval of scholarly texts.
#'
#' @description fulltext is a single interface to many sources of scholarly texts. In practice,
#' this means only ones that are legally useable. We will support sources that require
#' authentication on a case by case basis - that is, if more than just a few people 
#' will use it, and it's not too burdensome to include, then we can include that source.
#' 
#' @section What's included:
#' We currently include support for search and full text retrieval for a variety 
#' of publishers. See \code{\link{ft_search}} for what we include for search, and 
#' \code{\link{ft_get}} for what we include for full text retrieval.
#' 
#' @section Use cases:
#' The following are tasks/use cases supported:
#' \itemize{
#'  \item search - \code{\link{ft_search}}
#'  \item get texts - \code{\link{ft_get}}
#'  \item get full text links - \code{\link{ft_links}}
#'  \item extract text from pdfs - \code{\link{ft_extract}}
#'  \item serialize to different data formats - \code{\link{ft_serialize}}
#'  \item extract certain article sections (e.g., authors) - \code{\link{chunks}}
#'  \item grab supplementary materials for (re-)analysis of data - \code{\link{ft_get_si}}
#'  accepts article identifiers, and output from \code{\link{ft_search}} and 
#'  \code{\link{ft_get}}
#' }
#' 
#' @section DOI delays:
#' Beware that DOIs are not searchable via Crossref/Entrez immediately. The delay may 
#' be as much as a few days, though should be less than a day. This delay should become
#' shorter as services improve. The point of this is that you man not find a match 
#' for a relatively new DOI (e.g., for an article published the same day). We've 
#' tried to account for this for some publishers. For example, for Crossref we search
#' Crossref for a match for a DOI, and if none is found we attempt to retrieve the 
#' full text from the publisher directly. 
#' 
#' @section Feedback: 
#' Let us know what you think at \url{https://github.com/ropensci/fulltext/issues}
#' 
#' @importFrom utils URLdecode URLencode browseURL head modifyList download.file unzip
#' @importFrom methods is
#' @importFrom stats na.omit setNames
#' @importFrom xml2 read_html read_xml xml_find_one xml_find_all xml_text xml_contents xml_attr
#' xml_ns xml_children xml_name
#' @importFrom httr HEAD GET POST upload_file content_type content write_disk stop_for_status
#' @importFrom whisker whisker.render
#' @importFrom rentrez entrez_search entrez_fetch entrez_link
#' @name fulltext-package
#' @aliases fulltext
#' @docType package
#' @author Scott Chamberlain <myrmecocystus@@gmail.com>
#' @keywords package
NULL
