context('bootstrap_syberia_project')
library(testthatsomemore)

test_that('it can bootstrap a trivial syberia project correctly', {
  within_file_structure(list(config = list('application.R')), {
    assert(syberia_project(tempdir))
  })
})

test_that('it can bootstrap routes for a simple syberia project', {
  within_file_structure(list('foo.R', config = list('application.R', 'routes.R' = 
    deparse(quote(list(foo = function() 'test'))))), {
    d <- syberia_project(tempdir)
    expect_identical(d$resource('foo')$value(), 'test')
  })
})

test_that('it can define a controller through routes', {
  within_file_structure(list('foo.R', config = list('application.R', 'routes.R' = 
    "list(foo = 'test')"), lib = list(controllers = list(test.R = 'function() "test"'))), {
    d <- syberia_project(tempdir)
    expect_identical(d$resource('foo')$value(), 'test')
  })
})
