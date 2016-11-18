test_that("A call to `resource` with no parameters sources the tested resource", {
  expect_equal(resource(), "This resource will be tested.")  
})

test_that("A call to `resource` with a parameter sources the named resource", {
  expect_equal(resource("inner_resource"), "This resource got called in another resource's test")
})
