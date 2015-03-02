library(testthatsomemore)
context('syberia project tests')

old_context <- context
context <- function(...) {}

describe('One off testing', {
  test_that('it can execute a simple test correctly', {
    within_file_structure(list(foo.R = '"test"', config = list('application.R'),
      test = list(foo.R = deparse(quote({
        test_that("it can run a simple test", {
          expect_identical(resource(), 'test')
        })
      })))),
    {
      p <- syberia_project(tempdir)
      assert(p$resource('test/foo')$value())
    })
  })

  test_that('it can give the test environment access to the director', {
    within_file_structure(list(config = list('application.R',
        environments = list(test.R = 'd <- class(director)'))), {
      p <- syberia_project(tempdir)
      expect_equal(as.character(p$resource('config/environments/test')$value()$d), 'director')
    })
  })
})

describe('Running test_project', {
  test_that('it can run test_project on a simple project', {
    p <- syberia_project(file.path(getwd(), 'projects', 'simple_test'))
    assert(capture.output(test_project(p)))
  })
  
  test_that('it can run test_project on a simple project with one failing test', {
    with_test_mock({
      p <- syberia_project(file.path(getwd(), 'projects', 'simple_failing_test'))
      expect_error_(capture.output(test_project(p)))
    })
  })
})

# TODO: (RK) Test `base` parameter.

context <- old_context
