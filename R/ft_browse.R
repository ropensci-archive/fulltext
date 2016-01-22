#' Browse an article in your default browser
#'
#' @name ft_browse
#' @param x An object of class \code{ft_data} - the output from a call to 
#' \code{\link{ft_get}}
#' @param what (character) One of macrodocs (default), publisher, or whisker.
#' @param output A file path, if not given, uses a temporary file, deleted up on leaving the
#' R session.
#' @param browse (logical) Whether to browse (default) or not. If \code{FALSE},
#' return the url.
#'
#' @details
#' \code{what=whisker} not operational yet. When operational, will use whisker to open
#' html page from XML content, each section parsed into separate section.
#'
#' @examples \dontrun{
#' x <- ft_get('10.7554/eLife.04300', from='elife')
#' ft_browse(x)
#' ft_browse(x, browse=FALSE)
#'
#' ft_browse( ft_get('10.3389/fphar.2014.00109', from="entrez") )
#'
#' # open to publisher site
#' ft_browse(x, "publisher")
#'
#' # Browse sections
#' x <- ft_get(c('10.1371/journal.pone.0086169',"10.1371/journal.pone.0110535"), from='plos')
#' ft_browse_sections(x, "abstract")
#' ft_browse_sections(x, "categories")
#'
#' opts <- list(fq=list('doc_type:full',"article_type:\"research article\""))
#' out <- ft_search(query='ecology', from='plos', plosopts = opts)$plos$data$id %>%
#'  ft_get(from = "plos")
#' out %>% ft_browse_sections("abstract")
#' out %>% ft_browse_sections("body")
#' }

#' @export
#' @rdname ft_browse
ft_browse <- function(x, what = "macrodocs", browse = TRUE) {
  what <- match.arg(what, c("macrodocs","publisher","whisker"))
  if (!is(x, "ft_data")) stop("x must be of class ft_data", call. = FALSE)
  doi <- get_doi(x)
  url <- switch(what,
                macrodocs = paste0(md(), doi),
                publisher = paste0(dx(), doi),
                whisker = stop("not working yet :)", call. = FALSE))
  if (browse) browseURL(url) else url
}

md <- function() "http://macrodocs.org/?doi="
dx <- function() "http://dx.doi.org/"

get_doi <- function(x){
  tmp <- ft_compact(sapply(x, function(v){
    tmp <- v$opts$doi
    if (is.null(tmp)) v$opts$ids else tmp
  }))[[1]]
  if (length(tmp) == 0) {
    stop("No DOIs found", call. = FALSE)
  } else {
    tmp
  }
}

#' @export
#' @rdname ft_browse
ft_browse_sections <- function(x, what = "abstract", output=NULL, browse = TRUE) {
  origwhat <- what
  what <- match.arg(what, sections(), FALSE)
  what <- c("doi", what)
  input <- unname(chunks(x, what)[[1]])
  input <- lapply(input, function(x) setNames(x, c("doi","target")))
  for (i in seq_along(input)) {
    input[[i]] <- c(input[[i]], collapse = i)
  }
  input <- list(name = origwhat, data = input)
  rendered <- whisker.render(template, input)
  rendered <- gsub("&lt;em&gt;", "<b>", rendered)
  rendered <- gsub("&lt;/em&gt;", "</b>", rendered)
  if (is.null(output))
    output <- tempfile(fileext = ".html")
  write(rendered, file = output)
  if (browse) browseURL(output) else output
}

template <-
  '<!DOCTYPE html>
<head>
  <meta charset="utf-8">
  <title>fulltext - view stuff</title>
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta name="description" content="View stuff from articles">
  <meta name="author" content="fulltext">
  <!-- Le styles -->
	<link href="http://netdna.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css" rel="stylesheet">
  <link href="http://netdna.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.css" rel="stylesheet">
</head>

  <body>
    <div class="container">
    <center><h2>fulltext <i class="fa fa-arrow-circle-right"></i> {{name}}</h2></center>

      <div class="panel-group" id="accordion" role="tablist" aria-multiselectable="true">
      {{#data}}
        <div class="panel panel-default">
          <div class="panel-heading" role="tab" id="headingOne{{collapse}}">
            <h4 class="panel-title">
              <a data-toggle="collapse" data-parent="#accordion" href="#collapseOne{{collapse}}" aria-expanded="true" aria-controls="collapseOne{{collapse}}">
                {{doi}}
              </a>
            </h4>
          </div>
          <div id="collapseOne{{collapse}}" class="panel-collapse collapse in" role="tabpanel" aria-labelledby="headingOne{{collapse}}">
            <div class="panel-body"> <a href="http://dx.doi.org/{{doi}}" target="_blank" class="btn btn-info btn-xs" role="button"><i class="fa fa-link"></i></a> &nbsp;
              {{target}}
            </div>
          </div>
        </div>
      {{/data}}
      </div>

    <script src="http://code.jquery.com/jquery-2.0.3.min.js"></script>
    <script src="http://netdna.bootstrapcdn.com/bootstrap/3.3.1/js/bootstrap.min.js"></script>

  </body>
</html>'
