context("ft_get")

test_that("ft_get basic functionality works ...", {
  skip_on_cran()

  aa <- ft_get('10.1371/journal.pone.0086169', from = 'plos')

  # correct classes
  expect_is(aa, "ft_data")
  expect_is(aa$plos, "list")
  expect_is(aa$plos$found, "integer")
  expect_is(aa$plos$dois, "character")
  expect_is(aa$plos$data, "list")
  expect_is(aa$plos$data$data, "plosft")
  expect_null(aa$plos$data$backend)
  expect_is(aa$plos$data$data[[1]], "character")

  # correct dimensions
  expect_equal(aa$plos$data$path, "session")
  expect_equal(length(aa$plos$data$data), 1)
  expect_equal(length(aa$plos$data$data[[1]]), 1)
})

test_that("ft_get works for all data providers", {
  skip_on_cran()
  
  ## PeerJ
  bb <- ft_get('10.7717/peerj.228')
  ## eLife
  cc <- ft_get('10.7554/eLife.03032', from = "elife")
  ## BMC
  dd <- ft_get('10.1186/2049-2618-2-7', from = "bmc")
  ## FrontiersIn
  ee <- ft_get('10.3389/fphar.2014.00109')
  ## Hindawi - via Entrez
  ff <- ft_get('10.1155/2014/292109')
  ## F1000Research - via Entrez
  gg <- ft_get('10.12688/f1000research.6522.1')
  ## Pensoft
  hh <- ft_get('10.3897/zookeys.499.8360', from = "pensoft")
  ## Copernicus - via Entrez
  jj <- ft_get('10.5194/angeo-31-2157-2013')
  ## arXiv
  kk <- ft_get('cond-mat/9309029', from = "arxiv")
  ## bioRxiv
  mm <- ft_get('10.1101/012476', from = "biorxiv")
  ## Karger Publisher - via Entrez
  nn <- ft_get('10.1159/000369331')
  ## CogentOA Publisher - via Entrez
  oo <- ft_get('10.1080/23311916.2014.938430')
  
  expect_is(bb, "ft_data")
  expect_is(cc, "ft_data")
  expect_is(dd, "ft_data")
  expect_is(ee, "ft_data")
  expect_is(ff, "ft_data")
  expect_is(gg, "ft_data")
  expect_is(hh, "ft_data")
  expect_is(jj, "ft_data")
  expect_is(kk, "ft_data")
  expect_is(mm, "ft_data")
  expect_is(nn, "ft_data")
  expect_is(oo, "ft_data")
})

test_that("ft_get fails well", {
  skip_on_cran()

  expect_error(ft_get('0086169', from = 'plos'), "These are probably not DOIs")
  expect_error(ft_get('0086169', from = 'stuff'), "'arg' should be one")
})
