# Writing tests for adapters

Recall that [adapters](../../../lib/adapters) are responsible for reading
and writing data (or models). Since reading and writing are IO functions
that involve interacting with the outside world, testing adapters
will usually require good stubbing. Take a look through the
[testthatsomemore](http://github.com/robertzk/testthatsomemore) package
README for an introduction to stubbing.

For example, we know that the s3 adapter makes calls to 
[s3mpi](http://github.com/robertzk/s3mpi)'s `s3read` and `s3store`. However,
when writing a test, you should **never communicate with the outside world**.
Anything you need to test a resource should be self-contained within the project,
since you can't guarantee the continuous integration server will have
access to it otherwise. Here is a way we could stub tests for the s3 adapter.

```r
# test/lib/adapters/s3.R

test_that("it can write a data set to S3", {
  env <- new.env()
  package_stub("s3mpi", "s3store", function(obj, name) { env[[name]] <- obj }, {
    adapter <- resource()
    adapter$write(iris, 'test_key')
    expect_identical(env$test_key, iris,
      info = "iris should have been stored in the test_key in env")
  })
})

test_that("it can read a data set from S3", {
  env <- list2env(list(test_key = iris))
  package_stub("s3mpi", "s3read", function(name) { env[[name]] }, {
    adapter <- resource()
    expect_identical(adapter$read('test_key'), env$test_key,
      info = "iris should have been read from the test_key in env")
  })
})
```

We made use of [testthatsomemore](http://github.com/robertzk/testthatsomemore)'s
package_stub function to replace `s3mpi::s3read` and `s3mpi::s3store` for the
duration of the test. This way we avoided actually storing or reading objects
from S3, which would not be possible when running the tests in continuous integration.

Any external communication your adapter performs should be stubbed in the tests.

