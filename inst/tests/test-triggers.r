context("triggers functions testing")
require(syberia)

test_that("it checks for the base case", {
  expect_equal(class(trigger),"function")
})

test_that("it checks to see if all the right classes are being put through", {
  expect_equal(class(trigger(trigger)),c('syberiaTrigger', 'trigger','function'))
})

test_that("it tests to see if triggers is being inherited", {
  expect_true(is.trigger(trigger(trigger)))
})

test_that("it checks to see if the record function is working", {
  df <- data.frame(x = 1:10, y = letters[1:10])
  expect_equal(class(record(df)),c('syberiaTrigger', 'trigger','function'))
})