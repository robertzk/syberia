context('import stage')
library(s3mpi)

local({
  # Import stage doesn't usually run in CI when compiling models.
  old_val <- Sys.getenv("CI")
  on.exit(Sys.setenv(CI = old_val))
  Sys.setenv(CI = "")

  parameters_yield_data <- function(parameters, data) {
    sr <- stageRunner$new(env <- new.env(), resource()(env, parameters))
    sr$run(1)
    expect_identical(sr$context$data, data)
  }

  test_that('it can use the s3 option correctly', {
    package_stub('s3mpi', 's3read', function(...) ..1, {
      parameters_yield_data(list(s3 = 'hello'), 'hello')
    })
  })

  test_that('it can use the file option correctly', {
    file <- tempfile()
    write.csv(iris[-5], file, row.names = FALSE)
    parameters_yield_data(list(file = file), iris[-5])
    unlink(file)
  })

  test_that('it can use the R option correctly', {
    parameters_yield_data(list(R = 'iris'), iris)
  })
})
