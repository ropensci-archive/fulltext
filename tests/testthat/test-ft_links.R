context("ft_links")

test_that("ft_links", {
  skip_on_cran()

  vcr::use_cassette("ft_links_plos", {
    x <- ft_links("10.1371/journal.pone.0001248", from = "plos")
  })
  expect_is(x, "ft_links")
  expect_is(unclass(x), "list")
  expect_is(x$plos, "list")
  expect_equal(x$plos$found, 1)
  expect_equal(x$plos$ids, "10.1371/journal.pone.0001248")
  expect_is(x$plos$data, "list")
  expect_named(x$plos$data[[1]], c("xml", "pdf"))
  expect_match(x$plos$data[[1]]$xml, "manuscript")
  expect_match(x$plos$data[[1]]$pdf, "printable")
})

test_that("ft_links fails well", {
  skip_on_cran()
  
  expect_error(ft_links(), "no applicable method")
  expect_error(ft_links(5), "no applicable method")
  expect_error(ft_links("asdfas", from = "Asdfadfs"), "not in set")
})

test_that("ft_links curl options work", {
  skip_on_cran()

  # bmc
  vcr::use_cassette("ft_links_error_bmc", {
    expect_error(
      ft_links("10.1007/978-3-642-40455-9_52-1", from = "bmc", timeout_ms = 1),
      "[Tt]ime")
  })

  # crossref - curl timeout caught by tryCatch, returns nothing
  vcr::use_cassette("ft_links_error_crossref", {
    expect_equal(
      length(ft_links("10.1002/ecy.2629", from = "crossref", 
        timeout_ms = 1)$crossref$data),
      0)
  })

  # entrez - dont include, httr not in suggests
  # expect_error(
  #   ft_links("10.1099/mgen.0.000251", from = "entrez", config = httr::timeout(0.1)),
  #   "time")
})
