# pdf_info_via_gs() modified from tm:::pdf_info_via_gs
pdf_info_via_gs <- function(file){
  file <- normalizePath(file)
  gs_cmd <- find_gs_cmd(Sys.getenv("R_GSCMD", ""))
  out <- system2(gs_cmd, c("-dNODISPLAY -q", sprintf("-sFile=%s", 
        shQuote(file)), system.file("ghostscript", "pdf_info.ps", 
                                    package = "tm")), stdout = TRUE)
  out <- out[cumsum(out == "") == 2L][-1L]
  val <- sub("^[^:]+:[[:space:]]*", "", out)
  names(val) <- sub(":.*", "", out)
  val <- as.list(val)
  if (!is.null(d <- val$CreationDate)) 
    val$CreationDate <- PDF_Date_to_POSIXt(d)
  if (!is.null(d <- val$ModDate)) 
    val$ModDate <- PDF_Date_to_POSIXt(d)
  val
}

find_gs_cmd <- function(gs_cmd = ""){
  if (!nzchar(gs_cmd)) {
    if (.Platform$OS.type == "windows") {
      gsexe <- Sys.getenv("R_GSCMD")
      if (!nzchar(gsexe)) 
        gsexe <- Sys.getenv("GSC")
      gs_cmd <- Sys.which(gsexe)
      if (!nzchar(gs_cmd)) 
        gs_cmd <- Sys.which("gswin64c")
      if (!nzchar(gs_cmd)) 
        gs_cmd <- Sys.which("gswin32c")
      gs_cmd
    }
    else Sys.which(Sys.getenv("R_GSCMD", "gs"))
  }
  else Sys.which(gs_cmd)
}

# pdf_info_via_xpdf <- function(file, options = NULL){
#   outfile <- tempfile("pdfinfo")
#   on.exit(unlink(outfile))
#   status <- system2("pdfinfo", c(options, shQuote(normalizePath(file))), 
#                     stdout = outfile)
#   tags <- c("Title", "Subject", "Keywords", "Author", "Creator", 
#             "Producer", "CreationDate", "ModDate", "Tagged", "Form", 
#             "Pages", "Encrypted", "Page size", "File size", "Optimized", 
#             "PDF version")
#   re <- sprintf("^(%s)", 
#           paste(sprintf("%-16s", sprintf("%s:", tags)), collapse = "|"))
#   lines <- readLines(outfile, warn = FALSE)
#   ind <- grepl(re, lines)
#   tags <- sub(": *", "", substring(lines[ind], 1L, 16L))
#   info <- split(sub(re, "", lines), cumsum(ind))
#   names(info) <- tags
#   fmt <- "%a %b %d %X %Y"
#   if (!is.null(d <- info$CreationDate)) 
#     info$CreationDate <- strptime(d, fmt)
#   if (!is.null(d <- info$ModDate)) 
#     info$ModDate <- strptime(d, fmt)
#   if (!is.null(p <- info$Pages)) 
#     info$Pages <- as.integer(p)
#   info
# }

# pdf_info_via_xpdf() modified from tm:::pdf_info_via_xpdf
pdf_info_via_xpdf <- function(file, options = NULL){
  outfile <- tempfile("pdfinfo")
  on.exit(unlink(outfile))
  status <- system2("pdfinfo", c(options, shQuote(normalizePath(file))), 
                    stdout = outfile)
  lines <- readLines(outfile, warn = FALSE)
  tmp <- do.call("c", lapply(lines, function(x){
    x <- gsub("[^\x20-\x7F]", "", x) # remove unicode characters
    as.list(setNames(strtrim(sub("^:", "", strextract(x, ":\\s.+$"))), 
                     sub(":", "", strextract(x, "^[A-Za-z]+\\s?[A-Za-z]+:"))))
  }))
  fmt <- "%a %b %d %X %Y"
  modifyList(tmp, list(CreationDate = strptime(tmp$CreationDate, fmt), 
                       ModDate = strptime(tmp$ModDate, fmt),
                       Pages = as.integer(tmp$Pages)))
}

# pdf_text_via_gs() modified from tm:::pdf_text_via_gs
pdf_text_via_gs <- function(file){
  files <- normalizePath(file)
  gs_cmd <- find_gs_cmd(Sys.getenv("R_GSCMD", ""))
  tf <- tempfile("pdf")
  on.exit(unlink(tf))
  res <- system2(gs_cmd, c("-q -dNOPAUSE -dBATCH -P- -dSAFER -sDEVICE=ps2write", 
                           sprintf("-sOutputFile=%s", tf), "-c save pop -f", shQuote(file)))
  txt <- system2(gs_cmd, c("-q -dNODISPLAY -P- -dSAFER -dDELAYBIND -dWRITESYSTEMDICT -dSIMPLE", 
                           "-c save -f ps2ascii.ps", tf, "-c quit"), stdout = TRUE)
  if (any(grepl("Error handled by opdfread.ps", txt))) {
    stop(paste(c("Ghostscript failed, with output:", txt), 
               collapse = "\n"))
  }
  strsplit(paste(txt, collapse = "\n"), "\f")[[1L]]
}

# PDF_Date_to_POSIXt() modified from tm:::PDF_Date_to_POSIXt
PDF_Date_to_POSIXt <- function(s){
  s <- sub("^D:", "", s)
  s <- gsub("'", "", s)
  if (nchar(s) <= 14L) {
    s <- sprintf("%s%s", s, substring("    0101000000", nchar(s) + 
                                        1L, 14L))
    strptime(s, "%Y%m%d%H%M%S")
  }
  else if (substring(s, 15L, 15L) == "Z") {
    strptime(substring(s, 1L, 14L), "%Y%m%d%H%M%S")
  }
  else {
    strptime(s, "%Y%m%d%H%M%S%z")
  }
}
