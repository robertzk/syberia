context('export_stage')
library(s3mpi)

parameters_yield <- function(parameters, expr) {
  tmp <- new.env()
  if ("s3" %in% names(parameters)) 
    tmp$model_stage$model <- list(1)
  else
    tmp$model_stage$model <- 1
  sr <- stageRunner$new(tmp, resource()(parameters))
  sr$run()
  if (!missing(expr)) eval.parent(substitute(expr))
}

test_that('it can use the s3 option correctly', {
  marked <- FALSE
  package_stub('s3mpi', 's3store', function(...) marked <<- ..2, { 
    parameters_yield(list(s3 = 'boo'))
    expect_identical(marked, 'boo')
  })
})

test_that('it can use the file option correctly', {
  marked <- FALSE
  package_stub('base', 'saveRDS', function(...) marked <<- ..2, { 
    parameters_yield(list(file = 'bop'))
    expect_identical(marked, 'bop')
  })
})

test_that('it can use the R option correctly', {
  parameters_yield(list(R = '*tempvar*'))
  expect_identical(get('*tempvar*', envir = globalenv()), 1)
  rm('*tempvar*', envir = globalenv())
})

