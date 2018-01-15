ex <- function(str, pattern) regmatches(str, regexpr(pattern, str))
sm <- function(x) suppressMessages(x)
