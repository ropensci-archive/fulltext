context("ft_extract")

test_that("ft_extract returns...", {
  skip_on_cran()

  path <- system.file("examples", "example1.pdf", package = "fulltext")

  res <- ft_extract(path)

  # correct classes
  expect_is(res, "pdft_char")
  expect_is(res$meta, "list")
  expect_is(res$data, "character")

  # cleanup
  ## FIXME - probably within ft_extract should do cleanup...
  # unlink("../../inst/examples/example1.txt")
})

test_that("ft_extract fails well", {
  skip_on_cran()

  # bad path given
  expect_error(ft_extract(5), "no applicable method")
  expect_error(ft_extract('adfafsdf'), "File does not exist")
})
