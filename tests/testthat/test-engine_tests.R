# Beware. Here lie meta-tests.
context("test_engine")
library(testthatsomemore)

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

describe("passing tests", {
  test_that("it passes with a simple example test", {
    testthatsomemore::assert(test_engine("projects/test_simple_find"))
  })

  test_that("it passes with a calculation of pi", {
    testthatsomemore::assert(test_engine("projects/test_calculation_pi"))
  })

  test_that("tests can call resources other than the tested resource", {
    testthatsomemore::assert(test_engine("projects/test_resourcing_in_tests"))
  })

})

describe("failing tests", {
  has_failed_test <- function(test_summary) {
    any(vapply(test_summary, function(summand) {
      any(vapply(summand[[1L]]$results, function(result) {
        identical(result$passed, FALSE)
      }, logical(1)))
    }, logical(1)))
  }

  test_that("it fails with a simple example test", {
    # TODO: (RK) Prevent test suite reporter mangling.
    sink(tempfile()); on.exit(sink())
    expect_true(has_failed_test(test_engine("projects/simple_test_failure")))
  })
})

describe("setup hook", {
  test_that("it fails with a setup hook failure", {
    expect_error(test_engine("projects/test_simple_setup_failure"), "setup test hook failed")
  })
})

