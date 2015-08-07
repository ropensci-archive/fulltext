#' @title PDF extraction tools
#'
#' @description If you want to use \code{\link{ft_extract}} function, it currently has 
#' two options for how to extract text from PDFs: xpdf and ghostscript.
#' 
#' @section xpdf installation: 
#' See http://www.foolabs.com/xpdf/download.html for instructions on how to download 
#' and install `xpdf`. For OSX, you an also get `xpdf` via 
#' Homebrew (https://github.com/homebrew/homebrew-x11/blob/master/xpdf.rb) with 
#' \code{brew install xpdf}. Apparently, you can optionally install Poppler, which is 
#' built on xpdf. Get it at http://poppler.freedesktop.org/
#' 
#' @section ghostscript installation: 
#' See http://www.ghostscript.com/doc/9.16/Install.htm for instructions on how to 
#' download and install `ghostscript`. For OSX, you an also get `ghostscript` via 
#' Homebrew (https://github.com/Homebrew/homebrew/blob/master/Library/Formula/ghostscript.rb) 
#' with \code{brew install gs}
#' 
#' @name extract_tools
NULL
