#' fulltext package user settings
#' 
#' @export
#' @param cr_tdm_key Crossref TDM (text and data mining) API key. Retrive at
#' https://apps.crossref.org/clickthrough/researchers/
#' @examples \dontrun{
#' ft_setup(cr_tdm_key = "<mykey>")
#' ft_settings()
#' }
ft_setup <- function(cr_tdm_key = NULL, licenses = NULL) {
  if (!is.null(cr_tdm_key)) Sys.setenv(FT_CR_TDM_KEY = cr_tdm_key)
  if (!is.null(licenses)) Sys.setenv(FT_LICENSES = licenses)
}

#' @export
#' @rdname ft_setup
ft_settings <- function(x) {
  
}
