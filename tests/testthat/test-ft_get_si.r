context("ft_get_si")

test_that("ft_get_si returns...", {
  skip_on_cran()

  expect_true(file.exists(ft_get_si("10.1371/journal.pone.0127900", 1)))
  expect_true(file.exists(ft_get_si("10.1371/journal.pone.0127900", 1, "plos")))
  expect_true(file.exists(ft_get_si("10.1111/ele.12437", 1)))
  expect_true(file.exists(ft_get_si("10.6084/m9.figshare.979288", 1)))
  expect_true(file.exists(ft_get_si("E093-059", "myco_db.csv", "esa_archives")))
  expect_true(file.exists(ft_get_si("E092-201", "MCDB_communities.csv", "esa_data_archives")))
  expect_true(file.exists(ft_get_si("10.1126/science.1255768", "Appendix_BanksLeite_etal.txt")))
  expect_true(file.exists(ft_get_si("10.1098/rspb.2015.0338", vol=282, issue=1811, 1)))
})

test_that("ft_get_si fails well", {
  skip_on_cran()

  expect_error(ft_get_si('nonsense', 1))
})
