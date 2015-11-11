test_that("it returns a prediction", {
  mock_model <- resource()(default_args = list(
    formula = big_mpg ~ cyl + hp + gear,
    family = binomial(logit)))
  mtcars2 <- mtcars
  mtcars2$big_mpg <- mtcars$mpg > 20
  mock_model$train(mtcars2)
  expect_equal(length(mock_model$predict(mtcars2)), NROW(mtcars2))
})
