context("pdfx")

test_that("pdfx works to extract text form a good pdf", {
  path <- system.file("examples", "example2.pdf", package = "fulltext")
  res <- pdfx(file = path)
  
  # correct classes
  expect_is(res, "pdfx")
  expect_is(res$meta, "list")
  expect_named(res$meta, c('job', 'base_name', 'doi', 'warning'))
  expect_is(res$data, "xml_document")

  # pdfx works to get html
  res_html <- pdfx_html(res)
  expect_is(res_html, "xml_document")

  # pdfx works to get targz
  tarfile <- tempfile(fileext = ".tar.gz")
  tmp <- suppressMessages(pdfx_targz(res, write_path = tarfile))
  expect_null(tmp)
  expect_true(file.exists(tarfile))
  
  ffiles <- untar(tarfile, list = TRUE)
  expect_true(any(grepl("\\.pdfx\\.xml", ffiles)))
  
  unlink(tarfile) # cleanup
})

test_that("pdfx fails well", {
  expect_error(pdfx(file = "example1.pdf"), 
               "400 - No words could be identified in the document. Image-based documents such as scan outputs are not supported")
})
