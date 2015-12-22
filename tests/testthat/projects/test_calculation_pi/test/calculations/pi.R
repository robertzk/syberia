test_that("the calculation is close to pi", {
  # Note we have access to the testthat package.
  expect_less_than(abs(resource() - 3.1415926), 1e-3)
})

