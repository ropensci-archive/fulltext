load("../../ropenscilabs/parseids/data/dois.rda")
vec <- 581:630
dois[vec]
library(rcrossref)
library(dplyr)
df <- rcrossref::cr_works(dois[vec])
(res <- df$data[,c('publisher', 'member', 'DOI')] %>% arrange(member))
# (res <- df$data[,c('member', 'DOI')] %>% arrange(member))
df$data %>% group_by(publisher) %>% tally() %>% arrange(desc(n)) %>% data.frame

(fart <- ft_get(res$DOI))
# (fart <- ft_get(res$DOI[16:25], try_unknown=TRUE))


# out <- list()
# for (i in seq_along(res$DOI)) {
#   cat(i, "\n")
#   out[[i]] <- ft_get(res$DOI[i])
# }
# x <- res$DOI[34]
