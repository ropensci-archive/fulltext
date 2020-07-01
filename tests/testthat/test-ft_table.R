# delete all files before testing
ftxt_cache$delete_all()

test_that("ft_table", {
  skip_on_cran()

  # before any articles retrieved
  bef <- ft_table()

  expect_is(bef, "tbl")
  expect_equal(NROW(bef), 0)

  # retrieve an article
  invisible(sm(ft_get('10.7717/peerj.228')))

  # before any articles retrieved
  aft <- ft_table()

  expect_is(aft, "tbl")
  expect_equal(NROW(aft), 1)
  expect_equal(aft$dois, '10.7717/peerj.228')

  # before any articles retrieved
  aft_xml <- ft_table(type = "xml")
  aft_pdf <- ft_table(type = "pdf")
  expect_gt(NROW(aft_xml), NROW(aft_pdf))
})

test_that("ft_table fails well", {
  skip_on_cran()

  # doesn't exist
  expect_error(ft_table("asdfadf"))

  # path must be character
  expect_error(ft_table(5))

  # type must be chartr
  expect_error(ft_table(type = 5))

  # type must be one of
  expect_error(ft_table(type = 'foobar'))

  # xml_extract_text must be logical
  expect_error(ft_table(xml_extract_text = "asdfadf"))
})
