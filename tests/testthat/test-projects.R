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
    expect_equal(syberia_engine("projects/engines1/example")$resource("example_resource"),
                 "example_resource")
  })
})

