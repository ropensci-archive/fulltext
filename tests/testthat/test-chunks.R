context("ft_chunks")

# test_that("ft_chunks returns...", {
#   skip_on_cran()
# 
#   dois <- c('10.7554/elife.28589', '10.7554/elife.14009', '10.7554/elife.13941')
#   x <- suppressMessages(ft_get(dois))
#     
#   aa <- x %>% ft_collect() %>% ft_chunks("front")
#   bb <- x %>% ft_collect() %>% ft_chunks("body")
#   
#   # multiple things
#   cc <- x %>% ft_collect() %>% ft_chunks(c("doi","categories"))
#   
#   # tabularize
#   dd <- x %>% ft_collect() %>% ft_chunks(c("doi","history")) %>% ft_tabularize()
# 
#   # correct classes
#   expect_is(aa, "list")
#   expect_is(bb, "list")
#   expect_is(cc, "list")
#   expect_is(dd, "list")
#   
#   expect_is(aa$elife, "list")
#   expect_is(aa$elife[[1]], "list")
#   expect_named(aa$elife[[1]], "front")
#   
#   expect_named(bb$elife[[2]], "body")
#   
#   expect_named(cc$elife[[2]], c('doi', 'categories'))
#   
#   expect_is(dd$elife, 'data.frame')
# })

test_that("ft_chunks fails well", {
  skip_on_cran()

  res <- suppressMessages(ft_get('10.7554/elife.22170'))

  # bad path given
  expect_error(ft_chunks(), "\"x\" is missing")
  expect_error(ft_chunks('adfafsdf'), "Input to x must be of class ft_data")
  expect_error(ft_chunks(5), "Input to x must be of class ft_data")
  expect_error(ft_chunks(mtcars), "Input to x must be of class ft_data")
  expect_error(ft_chunks(res, "nada"), "'arg' should be one of")
})
