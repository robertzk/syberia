context("engine")

describe("Invalid inputs", {
  test_that("it errors when an non-engine directory is passed", {
    expect_error(syberia_engine(tempfile()), "No syberia engine")
  })

})
