context('ensure_no_global_variable_pollution')

suppressWarnings(rm('*test*', envir = globalenv()))

test_that('It can catch variable removal', {
  assign('*test*', 1, envir = globalenv())
  expect_error(ensure_no_global_variable_pollution(rm('*test*', envir = globalenv())), "were removed")
})

test_that('It can catch variable addition', {
  expect_error(ensure_no_global_variable_pollution(assign('*test*', 1, envir = globalenv())), "were added")
})

suppressWarnings(rm('*test*', envir = globalenv()))

