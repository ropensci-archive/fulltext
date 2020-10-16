library(rcrossref)
library(dplyr)
library(tidyr)
library(readr)

out <- list()
z <- cr_members(limit = 1)
total <- z$meta$total_results
times <- (floor(total / 1000) + 1)
for (i in seq_len(times)) {
  if (i == 1) {
    offset <- NULL
  } else {
    offset <- 1000 * (i - 1)
  }
  out[[i]] <- cr_members(limit = 1000, offset = offset)
}
length(out)
out[[1]]
out[[2]]
df <- dplyr::bind_rows(lapply(out, "[[", "data"))

length(df$id)
length(unique(df$id))

df2 <- select(df, id, prefixes, primary_name)
df3 <- mutate(df2, prefixes = strsplit(prefixes, ", ")) %>% 
  unnest(prefixes)
# %>% .$prefixes2 %>% .[1:100]

# write_csv(df3, "crossref_member_prefix.csv")
crossref_member_prefix <- df3
usethis::use_data(crossref_member_prefix, crossref_member_prefix,
  internal = TRUE, overwrite = TRUE)
# save(crossref_member_prefix, file = "data/crossref_member_prefix.RData",
#   version = 2)
