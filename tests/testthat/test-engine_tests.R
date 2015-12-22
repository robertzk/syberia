# Beware. Here lie meta-tests.
context("test_engine")

bag_of_objects <- list(force, 1, NULL, "foo", list(), new.env())

describe("invalid inputs", {
  lapply(Filter(Negate(is.character), bag_of_objects), function(object) {
    test_that(paste("it fails when test_engine is called with a", class(object)), {
      expect_error(test_engine(object), "Please pass a")
    })
  })
})

describe("missing tests", {
  test_that("it fails due to missing tests for the engines1 project", {
    expect_error(test_engine("projects/engines1/main"), "Tests are missing")
    expect_error(test_engine("projects/engines1/main"), "main_resource")
  })
})


