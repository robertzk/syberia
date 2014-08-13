context('syberia project tests')

old_context <- context
context <- function(...) {}

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

# TODO: (RK) Test `base` parameter.

context <- old_context
