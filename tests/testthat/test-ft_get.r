context("ft_get")

test_that("ft_get returns...", {
  skip_on_cran()

  aa <- ft_get('10.1371/journal.pone.0086169', from = 'plos')

  # correct classes
  expect_is(aa, "ft_data")
  expect_is(aa$plos, "list")
  expect_is(aa$plos$found, "integer")
  expect_is(aa$plos$dois, "character")
  expect_is(aa$plos$data, "list")
  expect_is(aa$plos$data$data, "plosft")
  expect_null(aa$plos$data$backend)
  expect_is(aa$plos$data$data[[1]], "character")

  # correct dimensions
  expect_equal(aa$plos$data$path, "session")
  expect_equal(length(aa$plos$data$data), 1)
  expect_equal(length(aa$plos$data$data[[1]]), 1)
})

test_that("ft_get fails well", {
  skip_on_cran()

  expect_error(ft_get('0086169', from = 'plos'), "These are probably not DOIs")
  expect_error(ft_get('0086169', from = 'stuff'), "'arg' should be one")
})
