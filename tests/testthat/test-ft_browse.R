context("ft_browse")

test_that("ft_browse returns...", {
  skip_on_cran()
  
  library("xml2")
  
  x <- ft_get('10.7554/eLife.04251', from = 'elife')
  aa <- ft_browse(x, browse = FALSE)
  aa_txt <- xml2::read_html(paste0(readLines(aa), collapse = ""))
  
  y <- ft_get('10.3389/fphar.2014.00109', from = "entrez")
  bb <- ft_browse(y, browse = FALSE)
  bb_txt <- xml2::read_html(paste0(readLines(bb), collapse = ""))
  
  cc <- ft_browse(x, "publisher", FALSE)
  
  # Browse sections
  z <- ft_get(c('10.1371/journal.pone.0086169',"10.1371/journal.pone.0110535"), from = 'plos')
  dd <- ft_browse_sections(z, "abstract", browse = FALSE)
  dd_txt <- xml2::read_html(paste0(readLines(dd), collapse = ""))
  ee <- ft_browse_sections(z, "categories", browse = FALSE)
  ee_txt <- xml2::read_html(paste0(readLines(ee), collapse = ""))
  
  # correct classes
  expect_is(aa, "character")
  expect_is(bb, "character")
  expect_is(cc, "character")
  expect_is(dd, "character")
  expect_is(ee, "character")
  
  expect_is(aa_txt, "xml_document")
  expect_is(bb_txt, "xml_document")
  expect_is(dd_txt, "xml_document")
  expect_is(ee_txt, "xml_document")
  
  expect_match(aa, "eLife")
  expect_match(bb, "fphar")
  expect_match(cc, "eLife")
  expect_equal(cc, "http://dx.doi.org/10.7554/eLife.04251")
  
  expect_match(xml_text(xml_children(xml_children(aa_txt)[[2]])[[1]]), "Macrodocs")
  
  expect_match(xml_text(xml_children(xml_children(bb_txt)[[2]])[[1]]), "Macrodocs")
  
  expect_match(xml_text(xml_children(xml_children(dd_txt)[[2]])), "abstract")
  
  expect_match(xml_text(xml_children(xml_children(ee_txt)[[2]])), "categories")
})

test_that("ft_browse fails well", {
  skip_on_cran()
  
  # bad path given
  expect_error(ft_browse(), "\"x\" is missing")
  expect_error(ft_browse('adfafsdf'), "x must be of class ft_data")
  expect_error(ft_browse(5), "x must be of class ft_data")
  expect_error(ft_browse(mtcars), "x must be of class ft_data")
})
