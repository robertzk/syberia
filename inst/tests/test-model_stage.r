context("model stage")

.num_rows <- 35
simple_model <- function(extra = function(env) {}) {
  modelenv <- new.env(); modelenv$data <- iris[seq_len(.num_rows), -5]
  modelenv$data[[1]] <- as.integer(modelenv$data[[1]] < 5)
  names(modelenv$data)[1] <- 'dep_var'
  extra(modelenv)
  fn <- model_stage(
    list('regularization', distribution = 'bernoulli',
         alpha = 0.5, prediction_type = 'response'))
  capture.output(fn(modelenv))
  modelenv
}

test_that("it sets the model object correctly", {
  sm <- simple_model()
  expect_is(sm$model_stage$model, 'tundraContainer')
  expect_is(sm$model_stage$model$output$model, 'cv.glmnet')
})

test_that("it can make a prediction after training", {
  set.seed(100)
  sm <- simple_model()
  expect_equal(sm$model_stage$model$predict(sm$data[1, ]), 0.3614491239243756703914)
})

test_that("it sets the munge_procedure on the model", {
  sm <- simple_model(function(env)
    attr(env$data, 'mungepieces') <-
      list(one = list(column_transformation(function(x) 2 * x), 1))
  )

  expect_identical(names(sm$model_stage$model$munge_procedure)[1], 'one')
})

test_that("it tracks variable summaries", {
  sm <- simple_model(function(env)
    env$import_stage$variable_summaries <-
      list(means = lapply(iris[seq_len(.num_rows), -5], mean))
  )

  expect_equal(sm$model_stage$model$internal$variable_summaries$means[[1]],
               mean(iris[seq_len(.num_rows), 2]))
})


