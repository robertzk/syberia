context('data stage')

parameters_yield_data <- function(parameters, data) {
  tmp <- as.environment(list(data = iris))
  sr <- resource()(tmp, parameters)
  sr$run()
  attr(sr$.context$data, 'mungepieces') <- NULL
  expect_identical(sr$.context$data, data)
}

test_that('it can correctly drop a variable', {
  parameters_yield_data(list(
    "Drop first variable" = list(drop_variables, 1)
  ), iris[-1])
})
