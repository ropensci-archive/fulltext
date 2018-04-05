context("ft_get_si")

# test_that("ft_get_si returns...", {
#   skip_on_cran()

#   #Each journal/publisher combo works well
#   expect_true(file.exists(ft_get_si("10.1371/journal.pone.0127900", 1)))
#   expect_true(file.exists(ft_get_si("10.1371/journal.pone.0127900", 1, "plos")))
#   # expect_true(file.exists(ft_get_si("10.1111/ele.12437", 1)))
#   expect_true(file.exists(ft_get_si("10.6084/m9.figshare.979288", 1)))
#   expect_true(file.exists(ft_get_si("10.6084/m9.figshare.979288", "analysis.R")))
#   expect_null(attr(ft_get_si("10.6084/m9.figshare.979288", 1),"suffix"))
#   expect_true(file.exists(ft_get_si("E093-059", "myco_db.csv", "esa_archives")))
#   expect_identical(attr(ft_get_si("E093-059", "myco_db.csv", "esa_archives"), "suffix"), "csv")
#   expect_true(file.exists(ft_get_si("E092-201", "MCDB_communities.csv", "esa_data_archives")))
#   expect_true(file.exists(ft_get_si("10.1126/science.1255768", "Appendix_BanksLeite_etal.txt")))
#   expect_true(file.exists(ft_get_si("10.1098/rspb.2015.0338", vol=282, issue=1811, 1)))
#   expect_true(file.exists(ft_get_si("10.1101/016386", 1)))
#   #expect_true(file.exists(ft_get_si("10.1371/journal.pone.0126524", "pone.0126524.g005.jpg", "epmc")))

#   #Extra checks for Wiley
#   expect_true(file.exists(ft_get_si("10.1111/ele.12437", si=1)))
#   expect_true(file.exists(ft_get_si("10.1111/ele.12437", si=2)))
#   expect_true(file.exists(ft_get_si("10.1002/ece3.1679", si=2)))
#   expect_error(ft_get_si('10.1111/ele.12437', si=3))
  

#   #Multiple downloads and ft_data are handled well
#   #expect_true(all(file.exists(ft_get_si(c("10.1101/016386", "10.1111/ele.12437"), si=1))))
#   expect_true(all(file.exists(ft_get_si(c("10.1101/016386", "10.1111/ele.12437"), si=2:1))))
#   expect_true(all(file.exists(ft_get_si(c("10.1101/016386", "10.1111/ele.12437"), si=1))))
#   expect_true(file.exists(ft_get_si(ft_search("beyond the edge with edam"),1)))
#   expect_true(all(file.exists(ft_get_si(ft_get(c("10.1371/journal.pone.0126524","10.1371/journal.pone.0126524")),1))))
# })

# test_that("ft_get_si fails well", {
#   skip_on_cran()

#   expect_error(suppressWarnings(ft_get_si('nonsense', 1)))
#   expect_error(ft_get_si('10.6084/m9.figshare.979288', 20))
#   expect_error(ft_get_si('10.6084/m9.figshare.979288', "does_exist.csv"))
# })

# test_that("ft_get_si - curl options work", {
#   skip_on_cran()
  
#   library("httr")
  
#   # unlink files
#   unlink(list.files(tempdir(), pattern = "pone", full.names = TRUE))
#   unlink(list.files(tempdir(), pattern = "979288", full.names = TRUE))
#   unlink(list.files(tempdir(), pattern = "rspb", full.names = TRUE))
  
#   expect_error(ft_get_si('10.6084/m9.figshare.979288', 2, cache=FALSE, config = timeout(0.0001)))
#   expect_output(ft_get_si("10.1371/journal.pone.0127900", 1, cache=FALSE, config = progress()), "Downloading")
#   expect_output(ft_get_si("10.1098/rspb.2015.0338", vol=282, issue=1811, 1, cache=FALSE, config = progress()), "100%")
# })
