context("ft_links")

test_that("ft_links fails well", {
  skip_on_cran()
  
  expect_error(ft_links(), "no applicable method")
  expect_error(ft_links(5), "no applicable method")
  expect_error(ft_links("asdfas", from = "Asdfadfs"), "not in set")
})

test_that("ft_links curl options work", {
  skip_on_cran()
  
  # plos - succeeds - no curl options used
  expect_is(
    ft_links("10.1371/journal.pone.0001248", from = "plos", timeout_ms=1),
    "ft_links")

  # bmc
  expect_error(
    ft_links("10.1007/978-3-642-40455-9_52-1", from = "bmc", timeout_ms = 1),
    "[Tt]ime")

  # crossref - curl timeout caught by tryCatch, returns nothing
  expect_equal(
    length(ft_links("10.1002/ecy.2629", from = "crossref", 
      timeout_ms = 1)$crossref$data),
    0)

  # entrez - dont include, httr not in suggests
  # expect_error(
  #   ft_links("10.1099/mgen.0.000251", from = "entrez", config = httr::timeout(0.1)),
  #   "time")
})
