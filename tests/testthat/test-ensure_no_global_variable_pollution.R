context('ensure no global variable pollution')

suppressWarnings(rm('*test*', envir = globalenv()))

test_that('It can catch variable removal', {
  assign('*test*', 1, envir = globalenv())
  expect_error(ensure_no_global_variable_pollution(rm('*test*', envir = globalenv())), "were removed")
})

test_that('It can catch variable addition', {
  expect_error(ensure_no_global_variable_pollution(assign('*test*', 1, envir = globalenv())), "were added")
})

suppressWarnings(rm('*test*', envir = globalenv()))

old_options <- options('digits')
on.exit(options(old_options))

test_that('It does not error on no global options changing', {
  assert(ensure_no_global_variable_pollution(check_options = TRUE, { NULL }))
})

test_that('It can detect global options changing', {
  expect_error(ensure_no_global_variable_pollution(check_options = TRUE, {
    options(list(digits = old_options$digits - 1))
  }, 'global options were modified'))
})

