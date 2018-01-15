context("ft_get")

test_that("ft_get basic functionality works ...", {
  skip_on_cran()

  aa <- sm(ft_get('10.7717/peerj.228'))

  # correct classes
  expect_is(aa, "ft_data")
  expect_is(aa$peerj, "list")
  expect_is(aa$peerj$found, "integer")
  expect_is(aa$peerj$dois, "character")
  expect_is(aa$peerj$data, "list")
  expect_null(aa$peerj$data$data)
  expect_equal(aa$peerj$data$backend, "ext")
  expect_is(aa$peerj$data$path, "list")
})

test_that("ft_get works for all data providers", {
  skip_on_cran()

  ## PLOS
  aa <- sm(ft_get(c('10.1371/journal.pone.0086169', '10.1371/journal.pbio.0000062')))
  ## PeerJ
  bb <- sm(ft_get('10.7717/peerj.228'))
  ## eLife
  cc <- sm(ft_get('10.7554/eLife.03032', from = "elife"))
  ## BMC
  #dd <- sm(ft_get('10.1186/2049-2618-2-7', from = "bmc"))
  ## FrontiersIn
  ee <- sm(ft_get('10.3389/fphar.2014.00109'))
  ## Hindawi - via Entrez
  ff <- sm(ft_get('10.1155/2014/292109'))
  ## F1000Research - via Entrez
  gg <- sm(ft_get('10.12688/f1000research.6522.1'))
  ## Pensoft
  ## FIXME, used to work, no mas
  #hh <- sm(ft_get('10.3897/zookeys.499.8360', from = "pensoft"))
  ## Copernicus - via Entrez
  ## jj <- sm(ft_get('10.5194/angeo-31-2157-2013'))
  ## arXiv
  kk <- sm(ft_get('cond-mat/9309029', from = "arxiv"))
  ## bioRxiv
  mm <- sm(ft_get('10.1101/012476', from = "biorxiv"))
  ## Karger Publisher - via Entrez
  nn <- sm(ft_get('10.1159/000369331'))
  ## CogentOA Publisher - via Entrez
  oo <- sm(ft_get('10.1080/23311916.2014.938430'))

  expect_is(aa, "ft_data")
  expect_is(bb, "ft_data")
  expect_is(cc, "ft_data")
  #expect_is(dd, "ft_data")
  expect_is(ee, "ft_data")
  expect_is(ff, "ft_data")
  expect_is(gg, "ft_data")
  #expect_is(hh, "ft_data")
  #expect_is(jj, "ft_data")
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
