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
  sr <- stageRunner$new(modelenv, build_export_stagerunner(list()), remember = TRUE)
  expect_is(sr, 'stageRunner')
})

test_that('it correctly builds a stagerunner for one data source', {
  correct_filename <- file.path(tempfile())
  other_env <- new.env()
  saveRDS <- function(...) other_env$file_written <- TRUE
  modelenv <- new.env()
  sr <- stageRunner$new(modelenv,
    build_export_stagerunner(list(file = correct_filename)), remember = TRUE)
  # Quick and dirty, replace saveRDS in front of its eyes
  environment(environment(sr$stages[[1]]$fn)$fn)$saveRDS <- saveRDS
  sr$run()
  expect_true(other_env$file_written)
})

context('export_stage')

test_that('it runs an example export stage correctly', {
  somefile <- tempfile()
  filename <- file.path(somefile)
  modelenv <- new.env()
  some_model_data <- list("this is the model", 5)
  modelenv$model_stage <- list(model = some_model_data)
  stageRunner$new(modelenv, export_stage(list(file = filename)), remember = TRUE)$run()
  expect_identical(readRDS(filename), some_model_data)
  unlink(somefile)
})

test_that('it runs an example export stage with a copy correctly', {
  somefile <- tempfile()
  filename <- file.path(somefile)
  modelenv <- new.env(); mock_globalenv <- new.env()
  some_model_data <- list("this is the model", 5)
  modelenv$model_stage <- list(model = some_model_data)
  sr <- stageRunner$new(modelenv,
    export_stage(list(file = filename, copy = 'global_copy_of_model')), remember = TRUE)
  environment(sr$stages[[1]]$fn)$globalenv <- function() mock_globalenv
  sr$run()
  expect_identical(mock_globalenv$global_copy_of_model, some_model_data)
  unlink(somefile)
})

