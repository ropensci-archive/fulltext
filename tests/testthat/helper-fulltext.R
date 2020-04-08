ex <- function(str, pattern) regmatches(str, regexpr(pattern, str))
sm <- function(x) suppressMessages(x)
sw <- function(x) suppressWarnings(x)

library("fulltext")
sm(cache_options_set(full_path = file.path(tempdir(), "fulltext_store")))
ft_init()
