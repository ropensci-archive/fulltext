skip_on_cran()

context("ftd_prefixes: all prefixes")
test_that("all prefixes", {
  skip_on_cran()
  
  a <- ftd_prefixes()
  
  expect_is(a, 'list')
  expect_is(a[[1]]$publisher, 'character')
  expect_is(a[[1]]$prefixes, 'character')
  expect_is(a[[1]]$urls, "list")
  expect_null(a[[1]]$journals)
  
  expect_equal(a[[1]]$publisher, "cogent")
  expect_equal(a[[2]]$publisher, "ssrn")
})

context("ftd_prefixes: a single member")
test_that("all prefixes", {
  skip_on_cran()
  
  a <- ftd_prefixes("10.2139")
  
  expect_is(a, 'list')
  expect_is(a$publisher, 'character')
  expect_is(a$prefixes, 'character')
  expect_is(a$urls, "list")
  expect_named(a)
  expect_equal(a$prefixes, "10.2139")
  expect_null(a$journals)
})

test_that("ftd_prefixes fails correctly", {
  skip_on_cran()
  
  expect_error(ftd_prefixes(4444))
})
