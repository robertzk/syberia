context('file adapter')
library(testthatsomemore)

test_that("can read from a file", {
  within_file_structure(list(), {
    write.csv(anscombe, path <- file.path(tempdir, 'anscombe.csv'), row.names = FALSE)
    expect_equal(resource()$read(path), anscombe)
  })
})

test_that("can write to a file", {
  within_file_structure(list(), {
    path <- file.path(tempdir, 'anscombe.csv') 
    resource()$write(anscombe, path)
    expect_equal(resource()$read(path), anscombe)
  })
})

