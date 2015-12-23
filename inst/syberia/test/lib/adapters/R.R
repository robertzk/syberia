test_that("it can read a global variable correctly", {
  expect_identical(iris, resource()$read('iris'))
})

test_that("it can write to a global variable correctly", {
  resource()$write(iris, "*tmpvar*")
  expect_identical(get("*tmpvar*", envir = globalenv()), iris)
  rm("*tmpvar*", envir = globalenv())
})

