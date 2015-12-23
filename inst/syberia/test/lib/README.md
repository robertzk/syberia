Writing tests for lib
==========

Imagine you have a resource in `lib/foo/bar.R`. You can place a test for that
resource in `test/lib/foo/bar.R` using `test_that` to make assertions:

```r
test_that("the resource behaves as expected", {
  expect_true(!is.null(resource()))
})
```

You can use `stest("lib/foo/bar")` to test just that resource, or run
tests on the whole project using `test_project()`.
