context("ft_serialize")

test_that("ft_serialize returns...", {
  skip_on_cran()

  dois <- c('10.1371/journal.pone.0087376','10.1371%2Fjournal.pone.0086169',
  '10.1371/journal.pone.0102976','10.1371/journal.pone.0105225',
  '10.1371/journal.pone.0102722','10.1371/journal.pone.0033693')
  res <- ft_get(dois, from='plos')

  # From XML to JSON
  tojson <- ft_serialize(res, to='json')
  tojsonparsed <- jsonlite::fromJSON(tojson$plos$data$data$`10.1371/journal.pone.0087376`)

  # Parse raw xml to XMLInternalDocument class
  toxml <- ft_serialize(res, to='xml')
  toxmlparsed <- toxml$plos$data$data[[1]]

  # To a list
  tolist <- ft_serialize(res, to='list')
  tolistparsed <- tolist$plos$data$data[[1]]

  # To various data stores on disk
  ## To an .Rds file
  tofile <- ft_serialize(res, to = 'file')
  tofileloc <- attr(tofile, "location")

  ## To local files using R.cache package
  torcache <- ft_serialize(res, to = 'rcache')
  torcacheloc <- attr(torcache, "location")

  # correct classes
  expect_is(tojson, "ft_parsed")
  expect_is(toxml, "ft_parsed")
  expect_is(tolist, "ft_parsed")
  expect_is(tofile, "ft_parsed")
  expect_is(torcache, "ft_parsed")
  
  expect_is(tojson$plos, "list")
  expect_is(tojson$plos$found, "integer")
  expect_is(tojson$plos$dois, "character")
  expect_is(tojson$plos$opts, "list")
  expect_is(tojson$plos$data, "list")
  expect_is(tojson$plos$data$data, "plosft")
  expect_is(tojson$plos$data$data[[1]], "json")

  expect_is(tojsonparsed, "list")
  expect_is(toxmlparsed, "xml_document")
  expect_is(tolistparsed, "list")
  expect_is(tofileloc, "character")
  expect_is(torcacheloc, "character")
  
  expect_match(tofileloc, "fulltext_cache")
  expect_match(torcacheloc, "Rcache")
})

test_that("ft_serialize fails well", {
  skip_on_cran()
  
  res <- ft_get('10.1371/journal.pone.0087376', from='plos')

  # bad path given
  expect_error(ft_serialize(), "\"x\" is missing")
  expect_error(ft_serialize('adfafsdf'), "Input to x must be one of")
  expect_error(ft_serialize(5), "Input to x must be one of")
  expect_error(ft_serialize(mtcars), "Input to x must be one of")
  expect_error(ft_serialize(res, "nada"), "'arg' should be one of")
})
