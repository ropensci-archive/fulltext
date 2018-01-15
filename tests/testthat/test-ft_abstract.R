context("ft_abstract")

test_that("ft_abstract basic functionality works - PLOS", {
  skip_on_cran()
  
  res <- ft_search(query = 'biology', from = 'plos', limit = 20, 
     plosopts = list(fq = list('doc_type:full', '-article_type:correction',
                    '-article_type:viewpoints')))
  dois <- res$plos$data$id
  aa <- ft_abstract(x = dois[1:5], from = "plos")
  
  expect_is(aa, "ft_abstract")
  expect_named(aa, c('plos', 'scopus', 'ma', 'crossref'))
  expect_is(aa$plos, "list")
  expect_is(aa$plos[[1]], "list")
  expect_named(aa$plos[[1]], c('doi', 'abstract'))
  expect_is(aa$plos[[1]]$abstract, 'character')
})

# FIXME: just test stuff that don't need IP address for
# test_that("ft_abstract basic functionality works - SCOPUS", {
#   skip_on_cran()
#   
#   opts <- list(key = Sys.getenv('ELSEVIER_SCOPUS_KEY'))
#   res <- ft_search(query = 'biology', from = 'scopus', scopusopts = opts, 
#     limit = 4)
#   ids <- ex(res$scopus$data$`dc:identifier`, "[0-9]+")
#   aa <- ft_abstract(x = ids, from = 'scopus', 
#     scopusopts = list(
#       key = Sys.getenv('ELSEVIER_SCOPUS_KEY'),
#       id_type = "scopus_id"
#     )
#   )
#   
#   expect_is(aa, "ft_abstract")
#   expect_named(aa, c('plos', 'scopus', 'ma'))
#   expect_is(aa$plos, "list")
#   expect_is(aa$plos[[1]], "list")
#   expect_named(aa$plos[[1]], c('doi', 'abstract'))
#   expect_is(aa$plos[[1]]$abstract, 'character')
# })


## Undo comments when microdemic new ver up on cran
# test_that("ft_abstract basic functionality works - Microsoft", {
#   skip_on_cran()
#   
#   key <- Sys.getenv("MICROSOFT_ACADEMIC_KEY")
#   res <- ft_search("Y=[2010, 2012)", from = "microsoft", 
#      maopts = list(key = key))
#   ids <- res$ma$data$Id
#   Sys.sleep(1)
#   aa <- ft_abstract(x = ids[1:2], from = "microsoft",
#     maopts = list(key = Sys.getenv('MICROSOFT_ACADEMIC_KEY')))
#   
#   expect_is(aa, "ft_abstract")
#   expect_named(aa, c('plos', 'scopus', 'ma', 'crossref'))
#   expect_is(aa$ma, "list")
#   expect_is(aa$ma[[1]], "list")
#   expect_named(aa$ma[[1]], c('id', 'abstract'))
# })

test_that("ft_abstract basic functionality works - Crossref", {
  skip_on_cran()
  
  res <- ft_search("ecology", from = "crossref", 
    crossrefopts = list(filter = c(has_abstract = TRUE)))
  ids <- res$crossref$data$doi
  aa <- ft_abstract(x = ids, from = "crossref")
  
  expect_is(aa, "ft_abstract")
  expect_named(aa, c('plos', 'scopus', 'ma', 'crossref'))
  expect_is(aa$crossref, "list")
  expect_is(aa$crossref[[1]], "list")
  expect_named(aa$crossref[[1]], c('id', 'abstract'))
  expect_is(aa$crossref[[1]]$abstract, "character")
})

test_that("ft_abstract fails well", {
  skip_on_cran()
  
  expect_error(ft_abstract(), "\"x\" is missing")
  expect_error(ft_abstract("Asdfadfd", from = "Asdfadfs"), 
               "'arg' should be one of")
})
