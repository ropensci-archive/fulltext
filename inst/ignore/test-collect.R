context("collect")

test_that("collect returns...", {
  skip_on_cran()

  dir <- tempdir()
  res <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rcache", path=dir)
  aa <- res %>% collect()
  bb <- res %>% collect() %>% text()

  # correct classes
  expect_is(res, "ft_data")
  expect_is(aa, "ft_data")
  expect_is(bb, "list")
  
  expect_is(aa$plos, "list")
  expect_is(res$plos$data, "list")
  expect_is(aa$plos$data, "list")
  expect_null(res$plos$data$data)
  expect_is(aa$plos$data$data, "plosft")

  expect_is(bb$plos, "list")
  expect_is(bb$plos[[1]], "character")
})

test_that("collect fails well", {
  skip_on_cran()

  res <- ft_get('10.1371/journal.pone.0086169', from='plos', cache=TRUE, backend="rds")

  expect_error(collect())
  expect_error(collect('adfafsdf'), "no applicable method")
  expect_error(collect(5), "no applicable method")
})
