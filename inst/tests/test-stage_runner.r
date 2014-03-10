context("stage_runner testing")
require(syberia)

test_that("it stops with wrong input, ie if input isn't a list", {
  a <- c(1,2,3)
  expect_error(stage_runner(a))
})

test_that("it stops with wrong input", {
  a <- c(1,2,3)
  expect_error(stage_runner(a))
})

# TODO: finish the rest of the tests