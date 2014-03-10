context("helpers functions testing")
require(syberia)

test_that("it checks that generates formula, generates a formula", {
  expect_equal(class(generate_formula("a","b")),"formula")
})

test_that("it checks that generates formula, generates a formula", {
  expect_equal(generate_formula("a","b"), a~b)
})