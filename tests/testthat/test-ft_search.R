context("ft_search")

test_that("ft_search returns...", {
  skip_on_cran()
  
  aa <- ft_search(query = 'ecology', from = 'plos')
  flds <- c('id','author','eissn','journal','counter_total_all','alm_twitterCount')
  bb <- ft_search(query = 'climate change', from = 'plos', plosopts = list(fl = flds))
  cc <- ft_search(query = 'ecology', from = 'crossref')
  dd <- ft_search(query = 'ecology', from = 'biorxiv')
  
  # correct classes
  expect_is(aa, "ft")
  expect_is(bb, "ft")
  expect_is(cc, "ft")
  expect_is(dd, "ft")
  
  expect_is(aa$plos, "ft_ind")
  expect_is(aa$bmc, "ft_ind")
  expect_is(bb$plos, "ft_ind")
  expect_is(cc$crossref, "ft_ind")
  expect_is(dd$biorxiv, "ft_ind")
  
  expect_is(aa$plos$found, "integer")
  expect_is(aa$plos$license, "list")
  expect_is(aa$plos$opts, "list")
  expect_is(aa$plos$data, "data.frame")
  expect_is(aa$plos$data$id, "character")
  
  expect_named(bb$plos$data, c("id", "alm_twitterCount", "counter_total_all", "journal", "eissn", "author"))
  
  expect_is(cc$crossref$data, "data.frame")
  expect_true(cc$crossref$opts$filter[[1]])
  
  expect_is(dd$biorxiv$data, "data.frame")
  expect_match(dd$biorxiv$data$url[1], "http")
})

test_that("ft_search works for larger requests", {
  skip_on_cran()
  
  res_entrez <- ft_search(query = 'ecology', from = 'entrez', limit = 200)
  expect_is(res_entrez, "ft")
  expect_is(res_entrez$entrez, "ft_ind")
  expect_equal(NROW(res_entrez$entrez$data), 200)
  
  res_plos <- ft_search(query = 'ecology', from = 'plos', limit = 200)
  expect_is(res_plos, "ft")
  expect_is(res_plos$plos, "ft_ind")
  expect_equal(NROW(res_plos$plos$data), 200)
  
  res_cr <- ft_search(query = 'ecology', from = 'crossref', limit = 200)
  expect_is(res_cr, "ft")
  expect_is(res_cr$crossref, "ft_ind")
  expect_equal(NROW(res_cr$crossref$data), 200)
  
  expect_error(ft_search(query = 'ecology', from = 'entrez', limit = 2000), 
               "HTTP failure 414, the request is too large")
  ## FIXME - add catches for plos, other sources
  expect_error(ft_search(query = 'ecology', from = 'crossref', limit = 2000), 
               "limit parameter must be 1000 or less")
})


test_that("ft_search fails well", {
  skip_on_cran()
  
  # no query given
  expect_error(ft_search(from = 'plos'), "argument \"query\" is missing")
  # bad source
  expect_error(ft_search("foobar", from = 'stuff'), "'arg' should be one of")
  # no data found, not error, but no data
  expect_message(ft_search(5, from = 'plos'), "Sorry, no data found")
  expect_equal(suppressMessages(ft_search(5, from = 'plos')$plos$found), 0)
})
