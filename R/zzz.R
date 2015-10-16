ft_compact <- function (l) Filter(Negate(is.null), l)

ft_trunc_mat <- function(x, n = NULL){
  rows <- nrow(x)
  if (!is.na(rows) && rows == 0)
    return()
  if (is.null(n)) {
    if (is.na(rows) || rows > 100) { n <- 10 }
    else { n <- rows }
  }
  df <- as.data.frame(head(x, n))
  if (nrow(df) == 0)
    return()
  #   is_list <- vapply(df, is.list, logical(1))
  #   df[is_list] <- lapply(df[is_list], function(x) vapply(x, ft_obj_type, character(1)))
  mat <- format(df, justify = "left")
  width <- getOption("width")
  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(values), nchar(names))
  cumw <- cumsum(w + 1)
  too_wide <- cumw[-1] > width
  if (all(too_wide)) {
    too_wide[1] <- FALSE
    df[[1]] <- substr(df[[1]], 1, width)
  }
  shrunk <- format(df[, !too_wide, drop = FALSE])
  needs_dots <- is.na(rows) || rows > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""), FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, .. = dots)
  }
  print(shrunk)
  if (any(too_wide)) {
    vars <- colnames(mat)[too_wide]
    types <- vapply(df[too_wide], ft_type_sum, character(1))
    var_types <- paste0(vars, " (", types, ")", collapse = ", ")
    cat(ft_wrap("Variables not shown: ", var_types), "\n", sep = "")
  }
}

ft_wrap <- function (..., indent = 0, width = getOption("width")){
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}

#' Type summary
#' @export
#' @keywords internal
ft_type_sum <- function (x) UseMethod("ft_type_sum")

#' @method ft_type_sum default
#' @export
#' @rdname ft_type_sum
ft_type_sum.default <- function (x) unname(abbreviate(class(x)[1], 4))

#' @method ft_type_sum character
#' @export
#' @rdname ft_type_sum
ft_type_sum.character <- function (x) "chr"

#' @method ft_type_sum Date
#' @export
#' @rdname ft_type_sum
ft_type_sum.Date <- function (x) "date"

#' @method ft_type_sum factor
#' @export
#' @rdname ft_type_sum
ft_type_sum.factor <- function (x) "fctr"

#' @method ft_type_sum integer
#' @export
#' @rdname ft_type_sum
ft_type_sum.integer <- function (x) "int"

#' @method ft_type_sum logical
#' @export
#' @rdname ft_type_sum
ft_type_sum.logical <- function (x) "lgl"

#' @method ft_type_sum array
#' @export
#' @rdname ft_type_sum
ft_type_sum.array <- function (x){
  paste0(NextMethod(), "[", paste0(dim(x), collapse = ","),
         "]")
}

#' @method ft_type_sum matrix
#' @export
#' @rdname ft_type_sum
ft_type_sum.matrix <- function (x){
  paste0(NextMethod(), "[", paste0(dim(x), collapse = ","),
         "]")
}

#' @method ft_type_sum numeric
#' @export
#' @rdname ft_type_sum
ft_type_sum.numeric <- function (x) "dbl"

#' @method ft_type_sum POSIXt
#' @export
#' @rdname ft_type_sum
ft_type_sum.POSIXt <- function (x) "time"

ft_obj_type <- function (x)
{
  if (!is.object(x)) {
    paste0("<", ft_type_sum(x), if (!is.array(x))
      paste0("[", length(x), "]"), ">")
  }
  else if (!isS4(x)) {
    paste0("<S3:", paste0(class(x), collapse = ", "), ">")
  }
  else {
    paste0("<S4:", paste0(is(x), collapse = ", "), ">")
  }
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
  stopifnot(is(x, "list") || is(x, "vector"))
  x <- vapply(x, URLdecode, "")
  res <- vapply(x, is_doi, logical(1))
  if (all(res)) {
    TRUE
  } else {
    stop("These are probably not DOIs:\n\n", paste0(names(res[!res]), "\n"), call. = FALSE)
  }
}

is_or <- function(x, clazzes) {
  if (!class(x) %in% clazzes) stop("Input to x must be one of class ", paste0(clazzes, collapse = ", "), call. = FALSE)
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

xml_node_parse <- function(x) {
  as.list(setNames(strtrim(xml_text(x)), xml_name(x)))
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
  setNames(x, tolower(names(x))) 
}
