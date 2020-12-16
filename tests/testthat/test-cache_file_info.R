context("cache_file_info")

# delete all files
ftxt_cache$delete_all()

test_that("cache_file_info - when cache dir empty", {
  skip_on_cran()
  
  aa <- cache_file_info()
  
  expect_is(aa, "list")
  expect_named(aa, c('xml_not_valid', 'xml_abstract_only', 'pdf_not_valid'))
  expect_equal(length(aa$xml_not_valid), 0)
  expect_equal(length(aa$xml_abstract_only), 0)
  expect_equal(length(aa$pdf_not_valid), 0)
})

test_that("cache_file_info - when cache dir not empty", {
  skip_on_cran()
  
  # download a file
  pap <- ft_get('10.7717/peerj.7755')

  # cache info
  aa <- cache_file_info()
  
  expect_is(aa, "list")
  expect_named(aa, c('xml_not_valid', 'xml_abstract_only', 'pdf_not_valid'))
  expect_equal(length(aa$xml_not_valid), 0)
  expect_equal(length(aa$xml_abstract_only), 0)
  expect_equal(length(aa$pdf_not_valid), 0)
})

test_that("cache_file_info fails well", {
  skip_on_cran()
  
  expect_error(cache_file_info(5), "unused argument")
})
