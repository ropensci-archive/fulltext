ft_compact <- function (l) Filter(Negate(is.null), l)

ft_wrap <- function (..., indent = 0, width = getOption("width")){
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}

pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

is_doi <- function(x) {
  grepl("[0-9]+\\.[0-9]+/.+", x)
}

check_dois <- function(x) {
  stopifnot(inherits(x, "list") || inherits(x, "character"))
  x <- vapply(x, utils::URLdecode, "")
  res <- vapply(x, is_doi, logical(1))
  if (all(res)) {
    TRUE
  } else {
    stop("These are probably not DOIs:\n\n", paste0(names(res[!res]), "\n"), call. = FALSE)
  }
}

is_or <- function(x, clazzes) {
  if (!inherits(x, clazzes)) {
    stop("Input to x must be one of class ", 
      paste0(clazzes, collapse = ", "), 
      call. = FALSE)
  }
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
        paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

assert_from <- function(x, options) {
  if (!is.null(x)) {
    if (!x %in% options) {
      stop("'", deparse(substitute(x)), "' not in set: ",
          paste0(options, collapse = ", "), call. = FALSE)
    }
  }
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

xml_node_parse <- function(x) {
  as.list(stats::setNames(strtrim(xml_text(x)), xml_name(x)))
}

# Modified from plyr::try_default
try_default_ <- function(expr, default, quiet = FALSE) {
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) NULL)
  }
  else {
    try(result <- expr)
  }
  result
}

# Modified from plyr::tryNULL
try_NULL <- function(expr) try_default_(expr, NULL, quiet = TRUE)

move_col <- function(x, y) x[ c(names(x)[-grep(y, names(x))], y) ]

names_lower <- function(x) {
  stats::setNames(x, tolower(names(x))) 
}

rbl <- function(x) {
  (xxxxx <- data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE)
  ))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
`%<|>%` <- function(x, y) if (length(x) == 0) y else x

httr_write_disk <- function(path, overwrite) {
  if (!overwrite && file.exists(path)) {
    stop("Path exists and overwrite is FALSE", 
      call. = FALSE)
  }
  structure(list(
    method = NULL,
    url = NULL,
    headers = NULL,
    fields = NULL,
    options = NULL,
    auth_token = NULL,
    output = structure(list(
      path = path,
      file = NULL
    ), class = c("write_disk", "write_function"))
  ), classs = "request")
}
