# From plyr::rbind.fill
rbind_fill <- function(...) {
  dfs <- list(...)
  if (length(dfs) == 0)
    return()
  if (is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
    dfs <- dfs[[1]]
  }
  dfs <- ft_compact(dfs)
  if (length(dfs) == 0)
    return()
  if (length(dfs) == 1)
    return(dfs[[1]])
  is_df <- vapply(dfs, is.data.frame, logical(1))
  if (any(!is_df)) {
    stop("All inputs to rbind_fill must be data.frames",
         call. = FALSE)
  }
  rows <- unlist(lapply(dfs, .row_names_info, 2L))
  nrows <- sum(rows)
  ot <- output_template(dfs, nrows)
  setters <- ot$setters
  getters <- ot$getters
  if (length(setters) == 0) {
    return(as.data.frame(matrix(nrow = nrows, ncol = 0)))
  }
  pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
  for (i in seq_along(rows)) {
    rng <- seq(pos[i, 1], length = pos[i, 2])
    df <- dfs[[i]]
    for (var in names(df)) {
      setters[[var]](rng, df[[var]])
    }
  }
  quickdf(lapply(getters, function(x) x()))
}

output_template <- function(dfs, nrows) {
  vars <- unique(unlist(lapply(dfs, base::names)))
  output <- vector("list", length(vars))
  names(output) <- vars
  seen <- rep(FALSE, length(output))
  names(seen) <- vars
  for (df in dfs) {
    matching <- intersect(names(df), vars[!seen])
    for (var in matching) {
      output[[var]] <- allocate_column(df[[var]], nrows,
                                       dfs, var)
    }
    seen[matching] <- TRUE
    if (all(seen))
      break
  }
  list(setters = lapply(output, `[[`, "set"), getters = lapply(output, `[[`, "get"))
}

allocate_column <- function (example, nrows, dfs, var) {
  a <- attributes(example)
  type <- typeof(example)
  class <- a$class
  isList <- is.recursive(example)
  a$names <- NULL
  a$class <- NULL
  if (is.data.frame(example)) {
    stop("Data frame column '", var, "' not supported by rbind_fill")
  }
  if (is.array(example)) {
    if (length(dim(example)) > 1) {
      if ("dimnames" %in% names(a)) {
        a$dimnames[1] <- list(NULL)
        if (!is.null(names(a$dimnames)))
          names(a$dimnames)[1] <- ""
      }
      df_has <- vapply(dfs, function(df) var %in% names(df),
                       FALSE)
      dims <- unique(lapply(dfs[df_has], function(df) dim(df[[var]])[-1]))
      if (length(dims) > 1)
        stop("Array variable ", var, " has inconsistent dims")
      a$dim <- c(nrows, dim(example)[-1])
      length <- prod(a$dim)
    }
    else {
      a$dim <- NULL
      a$dimnames <- NULL
      length <- nrows
    }
  }
  else {
    length <- nrows
  }
  if (is.factor(example)) {
    df_has <- vapply(dfs, function(df) var %in% names(df),
                     FALSE)
    isfactor <- vapply(dfs[df_has], function(df) is.factor(df[[var]]),
                       FALSE)
    if (all(isfactor)) {
      levels <- unique(unlist(lapply(dfs[df_has], function(df) levels(df[[var]]))))
      a$levels <- levels
      handler <- "factor"
    }
    else {
      type <- "character"
      handler <- "character"
      class <- NULL
      a$levels <- NULL
    }
  }
  else if (inherits(example, "POSIXt")) {
    tzone <- attr(example, "tzone")
    class <- c("POSIXct", "POSIXt")
    type <- "double"
    handler <- "time"
  }
  else {
    handler <- type
  }
  column <- vector(type, length)
  if (!isList) {
    column[] <- NA
  }
  attributes(column) <- a
  assignment <- make_assignment_call(length(a$dim))
  setter <- switch(handler, character = function(rows, what) {
    what <- as.character(what)
    eval(assignment)
  }, factor = function(rows, what) {
    what <- match(what, levels)
    eval(assignment)
  }, time = function(rows, what) {
    what <- as.POSIXct(what, tz = tzone)
    eval(assignment)
  }, function(rows, what) {
    eval(assignment)
  })
  getter <- function() {
    class(column) <<- class
    column
  }
  list(set = setter, get = getter)
}

make_assignment_call <- function (ndims) {
  assignment <- quote(column[rows] <<- what)
  if (ndims >= 2) {
    assignment[[2]] <- as.call(c(as.list(assignment[[2]]),
                                 rep(list(quote(expr = )), ndims - 1)))
  }
  assignment
}

quickdf <- function (list) {
  rows <- unique(unlist(lapply(list, NROW)))
  stopifnot(length(rows) == 1)
  names(list) <- make_names(list, "X")
  class(list) <- "data.frame"
  attr(list, "row.names") <- c(NA_integer_, -rows)
  list
}

make_names <- function (x, prefix = "X") {
  nm <- names(x)
  if (is.null(nm)) {
    nm <- rep.int("", length(x))
  }
  n <- sum(nm == "", na.rm = TRUE)
  nm[nm == ""] <- paste(prefix, seq_len(n), sep = "")
  nm
}
