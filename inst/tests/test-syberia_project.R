context('syberia_project')

test_that('it errors when we try to fetch a syberia project with a non-character', {
  expect_error(syberia_project(5), 'must specify a file')
})

test_that('it errors when we try to fetch a syberia project with a non-character', {
  expect_error(syberia_project(character(0)), 'Instead we got a')
  expect_error(syberia_project(character(3)), 'of length 3')
})

test_that('it can detect a syberia project and create a director', {

})

