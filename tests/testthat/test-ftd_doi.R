skip_on_cran()
skip_if_crossref_api_down()

# delate any cached files, start over for tests
ftdoi_cache$delete_all()

context("ftd_doi")

test_that("ftd_doi: karger", {
  # dois_karger=rcrossref::cr_members(127, works=TRUE, limit=5)$data$doi
  # save(dois_karger, file="tests/testthat/dois/dois_karger.rda", version=2)
  load("dois/dois_karger.rda")
  a <- ftd_doi(dois_karger)
  expect_is(a, 'data.frame')
  expect_equal(length(dois_karger), 5)
  expect_equal(NROW(a), 5)
  expect_equal(a$doi[1], dois_karger[1])
  expect_is(a$url, 'character')
  expect_match(a$url, 'https://www.karger.com')
})

test_that("ftd_doi: pensoft", {
  dois_pensoft=c('10.3897/zookeys.594.8768', '10.3897/mycokeys.54.34571',
    '10.3897/phytokeys.99.26489', '10.3897/subtbiol.13.6719')
  a <- ftd_doi(dois_pensoft)
  expect_is(a, 'data.frame')
  expect_equal(NROW(a), 8)
  expect_equal(a$doi[1], dois_pensoft[1])
  expect_is(a$url, 'character')
  expect_match(a$url, 'pensoft.net')
})

test_that("ftd_doi: frontiers", {
  # dois_frontiers=rcrossref::cr_members(1965, works=TRUE, limit=5)$data$doi
  # save(dois_frontiers, file="tests/testthat/dois/dois_frontiers.rda", version=2)
  load("dois/dois_frontiers.rda")
  a <- ftd_doi(dois_frontiers)
  expect_is(a, 'data.frame')
  expect_equal(NROW(a), 10)
  expect_equal(a$doi[1], dois_frontiers[1])
  expect_is(a$url, 'character')
  expect_match(a$url, 'frontiersin.org')
})

test_that("ftd_doi: PNAS", {
  # dois_pnas=rcrossref::cr_members(341, works=TRUE, limit=5)$data$doi
  # save(dois_pnas, file="tests/testthat/dois/dois_pnas.rda", version=2)
  load("dois/dois_pnas.rda")
  a <- ftd_doi(dois_pnas)
  expect_is(a, 'data.frame')
  expect_equal(NROW(a), 5)
  expect_equal(a$doi[1], dois_pnas[1])
  expect_is(a$url, 'character')
  expect_match(a$url, 'pnas.org')
})

test_that("ftd_doi fails correctly", {
  skip_on_cran()
  
  expect_error(ftd_doi())
})
