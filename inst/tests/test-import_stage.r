context('import_adapter')

test_that('it fetches the s3 adapter', {
  expect_match(paste(deparse(body(import_adapter('s3', list()))), collapse = "\n"), 's3')
})

test_that('it fetches the default adapter', {
  expect_match(paste(deparse(body(import_adapter('file', list()))), collapse = "\n"), 'read.csv')
})


