context("ft_get")

# delete all files before testing
ftxt_cache$delete_all()

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
  gg <- sw(sm(ft_get('10.12688/f1000research.6522.1')))
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
  # oo <- sm(ft_get('10.1080/23311916.2014.938430'))

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
  # expect_is(oo, "ft_data")
})

test_that("ft_get: > 1 from works", {
  skip_on_cran()
  skip_on_os("windows") # FIXME: not sure why, but his has failed on windows ci

  plos_dois <- c('10.1371/journal.pone.0086169', '10.1371/journal.pbio.0000062')
  aa <- sm(ft_get(plos_dois, from = c("plos", "entrez")))

  expect_is(aa, "ft_data")
  expect_is(aa$plos, "list")
  expect_match(aa$plos$data$path[[1]]$path, "xml")
  expect_match(aa$plos$data$path[[1]]$type, "xml")

  expect_is(aa$entrez, "list")
  expect_match(aa$entrez$data$path[[1]]$path, "xml")
  expect_match(aa$entrez$data$path[[1]]$type, "xml")
})


test_that("ft_get works for pdf for plos provider", {
  skip_on_cran()

  ## PLOS
  aa <- sm(ft_get('10.1371/journal.pone.0086169', type = "pdf"))

  expect_is(aa, "ft_data")
  expect_is(aa$plos, "list")
  expect_match(aa$plos$data$path[[1]]$path, "pdf")
  expect_match(aa$plos$data$path[[1]]$type, "pdf")
  expect_is(pdftools::pdf_text(aa$plos$data$path[[1]]$path),
    "character")
})



# THIS IS NO LONGER THE CASE, BELOW TEST IN ITS PLACE NOW
# this DOI is for an OA article, but the URL we get from Crossref doesn't work
# this one fails on the first try as it uses
# https://bsapubs.onlinelibrary.wiley.com/doi/full/10.3732/ajb.1700190
# on the first try, then runs again with
# https://bsapubs.onlinelibrary.wiley.com/doi/pdf/10.3732/ajb.1700190
# test_that("ft_get: wiley problems", {
#   skip_on_cran()

#   aa <- sw(sm(ft_get(x = '10.3732/AJB.1700190', from = "wiley")))

#   expect_is(aa, "ft_data")
#   expect_is(aa$wiley, "list")
#   expect_equal(aa$wiley$errors$error, NA_character_)
# })

test_that("ft_get: ajb via wiley", {
  skip_on_cran()

  # american j botany eg
  aa <- sw(sm(ft_get(x = '10.3732/AJB.1700190', from = "wiley")))

  expect_is(aa, "ft_data")
  expect_is(aa$wiley, "list")
  expect_equal(aa$wiley$errors$error, NA_character_)
})

test_that("ft_get fails well", {
  skip_on_cran()

  expect_error(ft_get('0086169', from = 'plos'), "These are probably not DOIs")
  expect_error(ft_get('0086169', from = 'stuff'), "'arg' should be one")
  expect_error(ft_get('0086169', progress = 5),
    "progress must be of class logical")

  # hits check_type fxn
  expect_error(ft_get('10.7554/eLife.03032', type = "foobar"),
    "parameter must be")

  # hits check_type fxn
  expect_error(ft_get('10.7554/eLife.03032', type = "plain"),
    "'type' for elife must be")
  expect_error(
    ft_get("10.1016/j.trac.2016.01.027", from = "elsevier", type = "add"),
    "must be")
})

test_that("ft_get errors slot", {
  skip_on_cran()

  res <- suppressWarnings(
    ft_get(c('10.7554/eLife.03032', '10.7554/eLife.aaaa', '10.3389/fphar.2024.00109'))
  )

  expect_named(res, c('frontiersin', 'elife'))

  expect_is(res$elife$errors, "data.frame")
  expect_true(is.na(res$elife$errors$error[1]))
  expect_match(res$elife$errors$error[2], "out of bounds")

  expect_is(res$frontiersin$errors, "data.frame")
  expect_match(res$frontiersin$errors$error, "was supposed to be")

  expect_error(ft_get('0086169', from = 'plos'), "These are probably not DOIs")
  expect_error(ft_get('0086169', from = 'stuff'), "'arg' should be one")
})

context("ft_get: progress bars")
test_that("ft_get: entrez", {
  skip_on_cran()

  ftxt_cache$delete_all()

  entrez_dois <- c('10.1186/2049-2618-2-7', '10.1186/2193-1801-3-7')
  # 1st run, get progress bar
  expect_output(
    ft_get(entrez_dois, from = "entrez", progress = TRUE),
    "==========="
  )
  # subsequent runs, also get progress bar
  expect_output(
    ft_get(entrez_dois, from = "entrez", progress = TRUE),
    "==========="
  )
  # if progress=FALSE, no bar, but do get path exists messages
  expect_message(
    ft_get(entrez_dois, from = "entrez", progress = FALSE),
    "path exists"
  )
})

test_that("ft_get: elife", {
  skip_on_cran()

  ftxt_cache$delete_all()

  elife_dois <- c('10.7554/eLife.04300', '10.7554/eLife.03032')
  # 1st run, get progress bar
  expect_output(
    ft_get(elife_dois, from='elife', progress = TRUE),
    "==========="
  )
  # subsequent runs, also get progress bar
  expect_output(
    ft_get(elife_dois, from='elife', progress = TRUE),
    "==========="
  )
  # if progress=FALSE, no bar, but do get path exists messages
  expect_message(
    ft_get(elife_dois, from='elife', progress = FALSE),
    "path exists"
  )
  # same if goes through get_unknown path
  expect_output(
    ft_get(elife_dois, progress = TRUE),
    "==========="
  )
})

# cleanup before running curl options checks
ftxt_cache$delete_all()

test_that("ft_get curl options work", {
  skip_on_cran()

  # plos
  out_plos <- sw(ft_get("10.1371/journal.pone.0001248", from = "plos",
      timeout_ms = 1))
  expect_match(out_plos$plos$errors$error, "was not found")

  # entrez - NOT QUITE WORKING YET
  # out_entrez <- ft_get('10.1186/2193-1801-3-7', from = "entrez",
  #   config = httr::timeout(0.1))
  # expect_is(out_entrez$plos$errors$error, "was not found")

  # elife
  out_elife <- sw(ft_get('10.7554/eLife.03032', from = "elife",
    timeout_ms = 1))
  expect_match(out_elife$elife$errors$error, "time")

  # pensoft
  out_pensoft <- sw(ft_get('10.3897/mycokeys.22.12528', from = "pensoft",
    timeout_ms = 1))
  expect_match(out_pensoft$pensoft$errors$error, "[Tt]ime")

  # arxiv
  out_arxiv <- sw(ft_get('cond-mat/9309029', from = "arxiv", timeout_ms = 1))
  expect_match(out_arxiv$arxiv$errors$error, "[Tt]ime")

  # biorxiv
  out_biorxiv <- sw(ft_get('10.1101/012476', from = "biorxiv", timeout_ms = 1))
  expect_match(out_biorxiv$biorxiv$errors$error, "[Tt]ime")

  # elsevier
  out_elsevier <- sw(ft_get("10.1016/j.trac.2016.01.027", from = "elsevier",
    timeout_ms = 1))
  expect_match(out_elsevier$elsevier$errors$error, "[Tt]ime")

  # wiley
  out_wiley <- sw(ft_get("10.1006/asle.2001.0035", from = "wiley",
    timeout_ms = 1))
  expect_match(out_wiley$wiley$errors$error, "[Tt]ime")
})

# cleanup - delete all files
ftxt_cache$delete_all()
