test_that("the calculation is close to pi", {
  # Note we have access to the testthat package.
  expect_lt(abs(resource() - 3.1415926), 1e-3)
})

