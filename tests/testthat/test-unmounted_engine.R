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

