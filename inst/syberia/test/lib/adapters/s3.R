# TODO: (RK) Write these.
library(testthatsomemore)

test_that("it can write a data set to S3", {
  env <- new.env()
  package_stub("s3mpi", "s3store", function(...) { env[[..2]] <- ..1 }, {
    adapter <- resource()
    adapter$write(iris, 'test_key')
    expect_identical(env$test_key, iris,
      info = "iris should have been stored in the test_key in env")
  })
})

test_that("it can read a data set from S3", {
  env <- list2env(list(test_key = iris))
  package_stub("s3mpi", "s3read", function(...) { env[[..1]] }, {
    adapter <- resource()
    expect_identical(adapter$read('test_key'), env$test_key,
      info = "iris should have been read from the test_key in env")
  })
})

