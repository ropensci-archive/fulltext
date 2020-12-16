# delete all files before testing
ftxt_cache$delete_all()

context("ft_collect/ft_text")
test_that("ft_collect", {
  skip_on_cran()

  x <- sm(ft_get('10.7717/peerj.7755'))
  
  # note that the data is not in the object, gives NULL
  expect_null(x$peerj$data$data)
  expect_match(x$peerj$data$path[[1]]$path, "xml")
  
  # Collect data from the .xml file
  y <- ft_collect(x)
  
  # note that the data is not in the object, gives NULL
  expect_is(y$peerj$data$data, "list")
  expect_is(y$peerj$data$data[[1]], "xml_document")

  # ft_text
  ft_x <- ft_text(x)
  ft_y <- ft_text(y)
  expect_true(all(vapply(ft_x, length, 1) == 0))
  expect_false(all(vapply(ft_y, length, 1) == 0))
  expect_is(ft_y$peerj, "list")
  expect_is(ft_y$peerj[[1]], "xml_document")
})

test_that("ft_collect fails well", {
  skip_on_cran()

  # no methods for other classes
  expect_error(ft_collect("asdfadf"))
  expect_error(ft_collect(5))
  expect_error(ft_collect(mtcars))
})

test_that("ft_text fails well", {
  skip_on_cran()

  # no methods for other classes
  expect_error(ft_text("asdfadf"))
  expect_error(ft_text(5))
  expect_error(ft_text(mtcars))
})
