#' Fulltext search and retrieval of scholarly texts.
#'
#' fulltext is a single interface to as many sources of scholarly texts as possible. In practice,
#' this means only ones that are legally used. We will support sources that require
#' authentication on a case by case basis - that is, if more than just a few people will use it,
#' then we can include that source.
#'
#' We currently include:
#' \itemize{
#'  \item PLOS - Public Library of Science
#'  \item eLife
#'  \item BMC - BioMed Central
#'  \item Crossref
#'  \item Entrez
#'  \item arXiv - ya know, that cool preprint server
#' }
#'
#' The following are tasks/use cases supported:
#' \itemize{
#'  \item search - \code{ft_search}
#'  \item get texts - \code{ft_get}
#'  \item serialize to different formats - \code{ft_serialize}
#'  \item extract text from pdfs - \code{ft_extract}
#' }
#'
#' Feedback! Let us know what you think at \url{https://github.com/ropensci/fulltext/issues}
#' @importFrom utils URLdecode browseURL head
#' @importFrom methods is
#' @importFrom stats na.omit setNames
#' @importFrom xml2 read_html read_xml xml_find_one xml_find_all xml_text xml_contents xml_attr
#' xml_ns xml_children xml_name
#' @importFrom httr HEAD GET POST upload_file content_type content write_disk stop_for_status
#' @importFrom whisker whisker.render
#' @name fulltext-package
#' @aliases fulltext
#' @docType package
#' @title Fulltext search and retrieval of scholarly texts.
#' @author Scott Chamberlain <myrmecocystus@@gmail.com>
#' @keywords package
NULL
