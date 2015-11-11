# You can use familiar testthat or testthatsomemore packages
test_that('it creates a factor var', {
  mb <- resource() # resource helper fetched the resources for which the test was written
  data <- data.frame(X = c(1,2,3), is_mister = c(T,F,F), is_master = c(F,T,F), is_miss = c(F,F,T))
  mp <- mungeplane(data)
  mb$run(mp)
  expect_true(is.factor(mp$data$title))
})
