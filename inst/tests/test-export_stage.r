context('export_adapter')

test_that('it fetches the s3 adapter', {
  expect_match(paste(deparse(body(export_adapter('s3'))), collapse = "\n"), 's3')
})

test_that('it fetches the default adapter', {
  expect_match(paste(deparse(body(export_adapter('file'))), collapse = "\n"), 'saveRDS')
})

context('build_export_stagerunner')

test_that('it returns a stageRunner', {
  modelenv <- new.env()
  sr <- build_export_stagerunner(modelenv, list())
  expect_is(sr, 'stageRunner')
})

test_that('it correctly builds a stagerunner for one data source', {
  correct_filename <- file.path(tempfile())
  other_env <- new.env()
  saveRDS <- function(...) other_env$file_written <- TRUE
  modelenv <- new.env()
  sr <- build_export_stagerunner(modelenv, list(file = correct_filename))
  # Quick and dirty, replace saveRDS in front of its eyes
  environment(environment(sr$stages[[1]])$fn)$saveRDS <- saveRDS
  sr$run()
  expect_true(other_env$file_written)
})

context('export_stage')

test_that('it runs an example export stage correctly', {
  # TODO: fill this in
})

test_that('it runs an example export stage with a copy correctly', {
  modelenv <- new.env()
  mock_globalenv <- new.env(); mock_globalenv$cached_data <- iris
  sr <- import_stage(modelenv, list(file = 'nonexistent', skip = 'cached_data'))
  environment(sr$stages[[1]])$globalenv <- function() mock_globalenv
  sr$run()
  expect_identical(modelenv$data, iris)
})
