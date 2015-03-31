context("preprocessors")
library(testthatsomemore)

test_that("it can use a preprocessor to inject dependencies into a resource", {
  within_file_structure(list(foo.R = 'test + 1', config = list('application.R', 'routes.R' = 
    "list(foo = 'test')"), lib = list(controllers = list(test.R = deparse(quote({
    preprocessor <- function() { source_env$test <- 1; source() }; function() output}))))), {
    p <- syberia_project(tempdir)
    expect_identical(p$resource('foo'), 2)
  })
})
