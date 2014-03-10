context("helpers functions testing")
require(syberia)

# generate_formula
test_that("it checks that generates_formula, generates a formula", {
  expect_equal(class(generate_formula("a","b")),"formula")
})
test_that("it checks that generates formula, generates a formula", {
  expect_equal(generate_formula("a","b"), a~b)
})

# numeric_to_factor
test_that("it checks that base case returns true", {
  expect_error(numeric_to_factor(list(),0))
})
test_that("it checks that it only takes numerics", {
  # why is this returning true:
  #    numeric_to_factor(c(1,2),2),factor(c("Missing","2"))
})
test_that("it checks that it returns a factor", {
  expect_true(is.factor(numeric_to_factor(1,1)))
})
