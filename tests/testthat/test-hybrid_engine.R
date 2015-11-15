context("hybrid engines")

# TODO: (RK) Test that a DAG of mounted engines and utility engines co-operate
# in harmony, even when some of the mounted engines share common utility
# engines.

describe("two mounted engines with two distinct utility engines, one each", {
  test_that("it can combine resources created from two mounted engines, each consuming a utility engine resource", {
    engine <- syberia_engine(file.path("projects", "two_mounted_engines_with_utility", "main"))
    expect_equal(engine$resource("hello_world"), "hello world")
  })
})


