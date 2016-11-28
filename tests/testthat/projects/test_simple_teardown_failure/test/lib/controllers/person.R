test_that("it can parse a simple person", {
  input <- list(first_name = "Foo", last_name = "Bar, Ph.D.")
  expect_equal(
    `environment<-`(resource()$parser, environment())(),
    utils::person("Foo", "Bar, Ph.D.")
  )
})

