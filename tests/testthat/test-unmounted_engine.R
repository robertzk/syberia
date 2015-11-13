context("unmounted engine")

describe("A simple non-mounted utility engine", {
  test_that("it can load a resource from another non-mounted engine", {
    expect_equal(syberia_engine("projects/utility_engines/main")$resource("hello"),
                 "hello world")
  })

  test_that("it cannot load a resource from another non-mounted engine without direct specification", {
    expect_error(syberia_engine("projects/utility_engines/main")$resource("hello_raw"),
                 "Cannot find resource")
  })
})

describe("Dual wielding utility engines", {
  test_that("it can dual load resources from utility engines", {
    expect_equal(syberia_engine("projects/two_utility_engines/main")$resource("hello"),
                 "hello world")
  })

  test_that("it does not mount utility engines in non-canonical fashion", {
    expect_error(syberia_engine("projects/two_utility_engines/main")$resource("reverse_hello"),
                 "No resource")
  })

  test_that("it does not mount utility engines", {
    expect_error(syberia_engine("projects/two_utility_engines/main")$resource("hello_raw"),
                 "Cannot find resource")
  })
})

