flds <- c('doi', 'container_issnl', 'container_name', 'publisher')
dois1 <- c('10.7554/eLife.03032', '10.7554/eLife.32763') # not found
dois2 <- c('10.7717/peerj.228','10.7717/peerj.234') # found

test_that("fat_cat_search_one", {
  skip_on_cran()

  one <- fat_cat_search_one(dois1, fields = flds, size = length(dois1))
  two <- fat_cat_search_one(dois2, fields = flds, size = length(dois2))

  expect_is(one, "data.frame")
  expect_equal(NROW(one), 2)
  expect_is(one$doi, "character")
  expect_equal(one$message[1], "not found")

  expect_is(two, "data.frame")
  expect_equal(NROW(two), 2)
  expect_is(two$doi, "character")
  expect_true(is.na(two$message[1]))
})

test_that("fat_cat_search", {
  skip_on_cran()

  one <- fat_cat_search(dois1)
  two <- fat_cat_search(dois2)

  expect_is(one, "list")
  expect_equal(length(one), 2)
  expect_named(one, NULL)
  expect_is(one[[1]]$doi, "character")
  expect_equal(one[[1]]$message, "not found")

  expect_is(two, "list")
  expect_equal(length(two), 2)
  expect_named(two, NULL)
  expect_is(two[[1]]$doi, "character")
  expect_true(is.na(two[[1]]$message))
})

test_that("get_publisher2", {
  skip_on_cran()

  one <- get_publisher2(dois1)
  two <- get_publisher2(dois2)

  expect_is(one, "list")
  expect_equal(length(one), 2)
  expect_named(one, dois1)
  expect_is(one[[1]], "character")
  expect_is(attr(one[[1]], "publisher"), "character")
  expect_match(attr(one[[1]], "publisher"), "elife")
  expect_equal(attr(one[[1]], "issn"), "")
  expect_equal(attr(one[[1]], "error"), "not found")

  expect_is(two, "list")
  expect_equal(length(two), 2)
  expect_named(two, dois2)
  expect_is(two[[1]], "character")
  expect_is(attr(two[[1]], "publisher"), "character")
  expect_match(attr(two[[1]], "publisher"), "peerj")
  expect_equal(attr(two[[1]], "issn"), "2167-8359")
  expect_true(is.na(attr(two[[1]], "error")))
})

test_that("make_doi_str", {
  aa <- make_doi_str(dois1)

  expect_is(aa, "character")
  expect_equal(length(aa), 1)
  expect_match(aa, "doi:\\(")
  expect_match(aa, dois1[1])
  expect_match(aa, dois1[2])
})

test_that("unknown_id", {
  aa <- unknown_id("foo bar")

  expect_is(aa, "character")
  expect_equal(length(aa), 1)
  expect_match(aa, "unknown")
  expect_match(attr(aa, "error"), "foo bar")
})

test_that("check_type", {
  expect_error(check_type(5), "'type' parameter must be character")
  expect_error(check_type('foo'), "'type' parameter must be")
  expect_null(check_type('xml'))
  expect_null(check_type('pdf'))
  expect_null(check_type('plain'))
})
