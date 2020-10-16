skip_on_cran()

context("ftd_members: all members")
test_that("all members", {
  skip_on_cran()
  
  a <- ftd_members()
  
  expect_is(a, 'list')
  expect_is(a[[1]]$publisher, 'character')
  expect_is(a[[1]]$prefixes, 'character')
  expect_null(a[[1]]$urls)
  expect_is(a[[1]]$journals, 'data.frame')
})

context("ftd_members: a single member")
test_that("all members", {
  skip_on_cran()
  
  a <- ftd_members(1965)
  
  expect_is(a, 'list')
  expect_is(a$publisher, 'character')
  expect_is(a$prefixes, 'character')
  expect_is(a$urls, "list")
  expect_null(a$journals)
})

test_that("ftd_members fails correctly", {
  skip_on_cran()
  
  expect_error(ftd_members(4444))
})
