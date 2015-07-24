context("ft_search")

test_that("ft_search returns...", {
  skip_on_cran()
  
  aa <- ft_search(query = 'ecology', from = 'plos')
  flds <- c('id','author','eissn','journal','counter_total_all','alm_twitterCount')
  bb <- ft_search(query = 'climate change', from = 'plos', limit = 500, plosopts = list(fl = flds))
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
  expect_match(dd$biorxiv$data$URL[1], "http")
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
