context("cache_options_get")
test_that("cache_options_get - onload conditions", {
  skip_on_cran()
  
  aa <- cache_options_get()
  
  expect_is(aa, "list")
  expect_named(aa, c('cache', 'backend', 'path', 'overwrite'))
  expect_is(aa$cache, 'logical')
  expect_is(aa$backend, 'character')
  expect_is(aa$path, 'character')
  expect_is(aa$overwrite, 'logical')

  # overwrite is FALSE by default
  expect_false(aa$overwrite)

  # default path contains 'fulltext' in it
  expect_match(aa$path, 'R/fulltext')

  # identical to what set returns
  expect_identical(cache_options_set(), aa)
})

context("cache_options_set")
test_that("cache_options_set - onload conditions", {
  skip_on_cran()
  
  aa <- cache_options_set(path = "foobar")
  expect_is(aa, "list")
  expect_named(aa, c('cache', 'backend', 'path', 'overwrite'))
  expect_match(aa$path, 'R/foobar')

  bb <- cache_options_set(overwrite = TRUE)
  expect_true(bb$overwrite)

  skip_on_os("windows")
  mypath <- tempdir()
  cache_options_set(full_path = mypath)
  cc <- cache_options_get()
  expect_match(cc$path, mypath)
})

test_that("cache_options_set fails well", {
  skip_on_cran()
  
  expect_error(cache_options_set(5), "class character")
  expect_error(cache_options_set(overwrite = "afd"), "class logical")
  expect_error(cache_options_set(full_path = 5), "class character")
  expect_message(cache_options_set("foo", full_path = "bar"), "ignoring")
})

# reset cache path
invisible(cache_options_set())
