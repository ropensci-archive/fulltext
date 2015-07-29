context("ft_extract_corpus")

test_that("ft_extract_corpus returns...", {
  skip_on_cran()
  
  path <- system.file("examples", "example1.pdf", package = "fulltext")
  res <- ft_extract_corpus(path, "xpdf")
  tmres <- tm::TermDocumentMatrix(res$data)
  
  # correct classes
  expect_true(file.exists(path))
  expect_is(path, "character")
  
  expect_is(res, "xpdf")
  expect_is(res$meta, "data.frame")
  expect_is(res$data, "VCorpus")
  
  expect_is(tmres, "TermDocumentMatrix")
  
  expect_is(tmres$dimnames$Terms, "character")
  expect_is(tmres$dimnames$Terms[1], "character")
})

test_that("ft_extract_corpus fails well", {
  skip_on_cran()
  
  # bad path given
  expect_error(ft_extract_corpus())
  expect_error(ft_extract_corpus(5), "invalid 'file' argument")
  expect_error(ft_extract_corpus('adfafsdf'), "These do not exist")
})
