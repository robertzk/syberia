context("engine")
library(testthatsomemore)

describe("Invalid inputs", {
  test_that("it errors when an non-engine directory is passed", {
    dir <- tempfile()
    unlink(dir, TRUE, TRUE)
    dir.create(dir)
    expect_error(syberia_engine(dir), "No syberia engine")
  })

  test_that("it can't find the engine if application.R is misspelled", {
    testthatsomemore::within_file_structure(list("config" = list("applicashun.r")), {
    expect_error(syberia_engine(tempdir), "No syberia engine")
    })
  })
})

describe("Finding the engine", {
  test_that("it can find an engine with an application.r file", {
    testthatsomemore::within_file_structure(list("config" = list("application.r")), {
      expect_false(is.null(syberia_engine(tempdir)))
    })
  })

  test_that("it can find an engine with an application.R file", {
    testthatsomemore::within_file_structure(list("config" = list("application.R")), {
      expect_false(is.null(syberia_engine(tempdir)))
    })
  })

  test_that("it can find an engine with an application/application.R file", {
    testthatsomemore::within_file_structure(list("config" = list("application" = list("application.R"))), {
      expect_false(is.null(syberia_engine(tempdir)))
    })
  })

  test_that("it can find an engine with an application/application.r file", {
    testthatsomemore::within_file_structure(list("config" = list("application" = list("application.r"))), {
      expect_false(is.null(syberia_engine(tempdir)))
    })
  })
            browser()

  test_that("it can find an engine from a nested directory", {
    testthatsomemore::within_file_structure(list("foo" = list("bar.R"), "config" = list("application.R")), {
      expect_false(is.null(syberia_engine(file.path(tempdir, "foo", "bar.R"))))
    })
  })
})

