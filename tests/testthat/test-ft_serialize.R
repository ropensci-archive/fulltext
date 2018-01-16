context("ft_serialize")

# test_that("ft_serialize returns...", {
#   skip_on_cran()
# 
#   dois <- c('10.7717/peerj.228', '10.7717/peerj.224', '10.7717/peerj.221')
#   res <- sm(ft_get(dois))
# 
#   # From XML to JSON
#   tojson <- ft_serialize(ft_collect(res), to='json')
#   tojsonparsed <- jsonlite::fromJSON(tojson$peerj$data$data$`10.7717/peerj.228`)
# 
#   # Parse raw xml to XMLInternalDocument class
#   toxml <- ft_serialize(ft_collect(res), to='xml')
#   toxmlparsed <- toxml$peerj$data$data[[1]]
# 
#   # To a list
#   tolist <- ft_serialize(ft_collect(res), to='list')
#   tolistparsed <- tolist$peerj$data$data[[1]]
# 
#   # correct classes
#   expect_is(tojson, "ft_parsed")
#   expect_is(toxml, "ft_parsed")
#   expect_is(tolist, "ft_parsed")
# 
#   expect_is(tojson$peerj, "list")
#   expect_is(tojson$peerj$found, "integer")
#   expect_is(tojson$peerj$dois, "character")
#   expect_is(tojson$peerj$opts, "list")
#   expect_is(tojson$peerj$data, "list")
#   expect_is(tojson$peerj$data$data[[1]], "json")
# 
#   expect_is(tojsonparsed, "list")
#   expect_is(toxmlparsed, "xml_document")
#   expect_is(tolistparsed, "list")
# })

test_that("ft_serialize fails well", {
  skip_on_cran()

  res <- sm(ft_get('10.7717/peerj.228'))

  # bad path given
  expect_error(ft_serialize(), "\"x\" is missing")
  expect_error(ft_serialize('adfafsdf'), "Input to x must be one of")
  expect_error(ft_serialize(5), "Input to x must be one of")
  expect_error(ft_serialize(mtcars), "Input to x must be one of")
  expect_error(ft_serialize(res, "nada"), "'arg' should be one of")
})
