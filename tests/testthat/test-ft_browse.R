context("ft_browse")

test_that("ft_browse returns...", {
  skip_on_cran()
  
  library("xml2")
  
  x <- suppressMessages(ft_get('10.7554/eLife.04251', from = 'elife'))
  aa <- ft_browse(x, browse = FALSE)
  aa_txt <- xml2::read_html(paste0(readLines(aa), collapse = ""))
  
  y <- suppressMessages(ft_get('10.3389/fphar.2014.00109', from = "entrez"))
  bb <- ft_browse(y, browse = FALSE)
  bb_txt <- xml2::read_html(paste0(readLines(bb), collapse = ""))
  
  expect_is(aa, "character")
  expect_is(bb, "character")
  
  expect_is(aa_txt, "xml_document")
  expect_is(bb_txt, "xml_document")
  
  expect_match(aa, "eLife")
  expect_match(bb, "fphar")
})

test_that("ft_browse fails well", {
  skip_on_cran()
  
  # bad path given
  expect_error(ft_browse(), "\"x\" is missing")
  expect_error(ft_browse('adfafsdf'), "x must be of class ft_data")
  expect_error(ft_browse(5), "x must be of class ft_data")
  expect_error(ft_browse(mtcars), "x must be of class ft_data")
})
