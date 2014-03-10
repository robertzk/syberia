context("cache functions testing")
require(syberia)

test_that("it sets and gets the correct name with base case", {
  mycache <- cache()
  expect_equal(mycache$getNames(),NULL)
})

test_that("it sets and gets the correct value with base case", {
  mycache <- cache()
  mycache$set(NULL)
  expect_equal(mycache$get(),NULL)
})

test_that("it sets and gets the correct value with 1 val", {
  mycache <- cache()
  mycache$set(2,key="two")
  expect_equal(mycache$get()$two,2)
})

test_that("it sets and gets the correct names with 4 val", {
  mycache <- cache()
  mycache$set(1,key="one")
  mycache$set(2,key="two")
  mycache$set(3,key="three")
  mycache$set(4,key="four")
  keys <- c("one","two","three","four")
  expect_equal(mycache$getNames(),keys)
})

test_that("it sets and gets the correct values with 2 vals", {
  mycache <- cache()
  mycache$set(1,key="one")
  mycache$set(2,key="two")
  expect_equal(1,mycache$get()$one)
  expect_equal(2,mycache$get()$two)
})

test_that("it returns a list", {
  scache <- cache()
  expect_true(is.list(scache$get()))
})