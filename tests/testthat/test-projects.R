context("syberia projects")
library(testthatsomemore)

describe("Project example1", {
  test_that("it can load the syberia engine representing example1", {
    assert(syberia_engine("projects/engines1/main"))
  })

  test_that("it can load a primary resource", {
    expect_equal(syberia_engine("projects/engines1/main")$resource("main_resource"),
                 "main_resource")
  })

  test_that("it can load a secondary resource", {
    expect_equal(syberia_engine("projects/engines1/main")$resource("example_resource"),
                 "example_resource")
  })
})

describe("Project example2", {
  test_that("it can overwrite an earlier resource", {
    expect_equal(syberia_engine("projects/engines2/main")$resource("example_resource"),
                 "overwritten")
  })
})

describe("Project example_controller", {
  test_that("it can apply a simple controller with the base engine", {
    expect_equal(syberia_engine("projects/example_controller")$resource("example"),
                 list(first = 1, second = 2))
  })

  test_that("it can apply a simple controller with a preprocessor", {
    expect_equal(syberia_engine("projects/example_controller")$resource("simple_preprocessor"),
                 list(first = 1, second = 2))
  })

  test_that("it has access to the director object", {
    dir <- file.path(getwd(), "projects", "example_controller")
    expect_equal(syberia_engine("projects/example_controller")$resource("director"), dir)
  })
})

