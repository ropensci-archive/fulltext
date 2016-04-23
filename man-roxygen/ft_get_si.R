#' @details The examples probably give the best indication of how to
#' use this function. In general, just specify the DOI of the article
#' you want to download data from, and the number of the supplement
#' you want to download (1, 5, etc.). ESA journals don't use DOIs
#' (give the article code; see below), and Proceedings, Science, and
#' ESA journals need you to give the filename of the supplement to
#' download. For FigShare articles, you can give either the number or
#' the name. The file extensions (suffixes) of files are returned as
#' \code{suffix} attributes (see first example), which may be useful
#' if you don't know the format of the file you're downloading.
#'
#' For any DOIs not recognised (and if asked) the European PubMed
#' Central API is used to look up articles. What this database calls a
#' supplementary file varies by publisher; often they will simply be
#' figures within articles, but we (obviously) have no way to check
#' this at run-time. I strongly recommend you run any EPMC calls with
#' \code{list=TRUE} the first time, to see the filenames that EPMC
#' gives supplements, as these also often vary from what the authors
#' gave them. This may actually be a 'feature', not a 'bug', if you're
#' trying to automate some sort of meta-analysis.
#'
#' Below is a list of all the publishers this supports, and examples
#' of articles from them. I'm aware that there isn't perfect overlap
#' between these publishers and the rest of the package; I plan to
#' correct this in the near future.
#'
#' \describe{
#' \item{auto}{Default. Use a cross-ref search
#' (\code{\link[rcrossref:cr_works]{cr_works}}) on the DOI to
#' determine the publisher.}
#' \item{plos}{Public Library of Science journals (e.g., PLoS One;
#' \url{http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0126524})}
#' \item{wiley}{Wiley journals, (e.g.,
#' \url{http://onlinelibrary.wiley.com/doi/10.1111/ele.12289/abstract}}
#' \item{science}{Science magazine (e.g.,
#' \url{http://www.sciencemag.org/content/345/6200/1041.short})}
#' \item{proceedings}{Royal Society of London journals (e.g.,
#' \url{http://rspb.royalsocietypublishing.org/content/282/1814/20151215}). Requires
#' \code{vol} and \code{issue} of the article.}
#' \item{figshare}{Figshare, (e.g.,
#' \url{http://bit.ly/figshare-example})}
#' \item{esa_data_archives & esa_data}{You must give article codes,
#' not DOIs, for these, which you can find on the article itself. An
#' ESA Data Archive paper - not to be confused with an ESA Archive,
#' which is the supplement to an ESA paper. The distinction seems less
#' crazy once you're reading the paper - if it only describes a
#' dataset, it's an \code{esa_archive} paper, else it's an
#' \code{esa_data_archive}. For example,
#' \url{http://www.esapubs.org/archive/ecol/E092/201/default.htm} is
#' an \code{esa_data_archive} whose article code is E092-201-D1;
#' \url{http://esapubs.org/Archive/ecol/E093/059/default.htm} is a
#' \code{esa_archive} whose code is E093-059-D1.}
#' \item{biorxiv}{Load from bioRxiv (e.g.,
#' \url{http://biorxiv.org/content/early/2015/09/11/026575})} 
#' \item{epmc}{Look up an article on the Europe PubMed Central, and
#' then download the file using their supplementary materials API
#' (\url{http://europepmc.org/restfulwebservice}). See comments above
#' in 'notes' about EPMC.}
#' }
