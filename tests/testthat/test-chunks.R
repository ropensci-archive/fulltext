context("chunks")

test_that("chunks returns...", {
  skip_on_cran()

  library("rplos")
  (dois <- searchplos(q="*:*", fl='id',
     fq=list('doc_type:full',"article_type:\"research article\""), limit=5)$data$id)
  x <- ft_get(dois, from="plos")
    
  aa <- x %>% chunks("front")
  bb <- x %>% chunks("body")
  
  # multiple things
  cc <- x %>% chunks(c("doi","categories"))
  
  # tabularize
  dd <- x %>% chunks(c("doi","history")) %>% tabularize()

  # correct classes
  expect_is(aa, "list")
  expect_is(bb, "list")
  expect_is(cc, "list")
  expect_is(dd, "list")
  
  expect_is(aa$plos, "list")
  expect_is(aa$plos[[1]], "list")
  expect_named(aa$plos[[1]], "front")
  
  expect_named(bb$plos[[2]], "body")
  
  expect_named(cc$plos[[2]], c('doi', 'categories'))
  
  expect_is(dd$plos, 'data.frame')
})

test_that("chunks fails well", {
  skip_on_cran()

  res <- ft_get('10.1371/journal.pone.0087376', from='plos')

  # bad path given
  expect_error(chunks(), "\"x\" is missing")
  expect_error(chunks('adfafsdf'), "Input to x must be of class ft_data")
  expect_error(chunks(5), "Input to x must be of class ft_data")
  expect_error(chunks(mtcars), "Input to x must be of class ft_data")
  expect_error(chunks(res, "nada"), "'arg' should be one of")
})
