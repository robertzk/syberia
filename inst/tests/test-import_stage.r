context('import_adapter')

test_that('it fetches the s3 adapter', {
  expect_match(paste(deparse(body(import_adapter('s3'))), collapse = "\n"), 's3')
})

test_that('it fetches the default adapter', {
  expect_match(paste(deparse(body(import_adapter('file'))), collapse = "\n"), 'read.csv')
})

context('build_import_stagerunner')

test_that('it returns a stageRunner', {
  modelenv <- new.env()
  sr <- build_import_stagerunner(modelenv, list())
  expect_is(sr, 'stageRunner')
})

test_that('it correctly builds a stagerunner for one data source', {
  correct_filename <- file.path(tempfile())
  read.csv <- function(filename, ...) if (filename == correct_filename) iris
  modelenv <- new.env()
  sr <- build_import_stagerunner(modelenv, list(file = correct_filename))
  # Quick and dirty, replace read.csv in front of its eyes
  environment(environment(sr$stages[[1]])$fn)$read.csv <- read.csv
  sr$run()
  expect_identical(modelenv$data, iris)
})

test_that('it correctly checks whether no data source loaded correctly', {
  modelenv <- new.env()
  sr <- build_import_stagerunner(modelenv, list())
  expect_error(sr$run(), 'Failed to load data from all data sources')
})

test_that('it skips data sources it can\'t load from', {
  correct_filename <- file.path(tempfile())
  read.csv <- function(filename, ...) if (filename == correct_filename) iris else stop('error')
  modelenv <- new.env()
  opts <- normalize_import_options(
    list(source1 = list(file = 'nonexistent'), source2 = list(file = correct_filename))
  )
  sr <- build_import_stagerunner(modelenv, opts)
    
  # Quick and dirty, replace read.csv in front of its eyes
  lapply(seq_len(length(sr$stages) - 1),
         function(ix) environment(environment(sr$stages[[ix]])$fn)$read.csv <<- read.csv)
  sr$run()
  expect_identical(modelenv$data, iris)
})

context('import_stage')

test_that('it runs an example import stage correctly', {
  modelenv <- new.env()
  file <- tempfile()
  write.csv(iris, file, row.names = FALSE)
  filename <- file.path(file)
  import_stage(modelenv, list(file = filename))$run()
  modelenv$data[[5]] <- factor(modelenv$data[[5]],
    levels = levels(iris[[5]]), labels = levels(iris[[5]]))
  expect_identical(modelenv$data, iris)
  unlink(file)
})

