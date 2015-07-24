context("ft_providers")

test_that("ft_providers returns...", {
  skip_on_cran()
  
  aa <- ft_providers(journal="Stem Cells International")
  bb <- ft_providers(publisher="hindawi")
  cc <- ft_providers(publisher="journal")
  
  # correct classes
  expect_is(aa, "ft_p")
  expect_is(bb, "ft_p")
  expect_is(cc, "ft_p")
  
  expect_is(aa$meta, "data.frame")
  expect_is(bb$meta, "data.frame")
  expect_is(cc$meta, "data.frame")
  
  expect_is(aa$data, "data.frame")
  expect_is(bb$data, "data.frame")
  expect_is(cc$data, "data.frame")
  
  expect_null(bb$facets)
  expect_null(cc$facets)
  
  # correct dimensions
  expect_true(any(grepl("Stem Cells International", aa$data$title)))
  expect_true(any(grepl("Hindawi", bb$data$primary_name)))
})

test_that("ft_providers fails well", {
  skip_on_cran()
  
  # no input given
  expect_error(ft_providers(), "Provide either journal or publisher")
})
