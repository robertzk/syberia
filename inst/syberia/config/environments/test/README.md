The test environment
=======

The `test.R` file is used to configure tests for a Syberia project.

There are several important concepts to understand about this process. First,
writing `test_project('path/to/project')` (or wherever you have placed
a local copy of this repository) will run *unit tests* for all Syberia resources:
models, stages, adapters, classifiers, mungebits, etc.

When running unit tests, we may want a few things about our global environment or
loaded packages to look slightly different than usual (e.g., set up some
dependencies or mock some packages that reach out to external sources).

To do this, Syberia offers `setup` and `teardown` hooks.

Test setup and teardown
--------

To execute some code before and after all tests are run, we can define
a function `setup` and/or `teardown` in the `test.R` file in this directory.
For example, if we write

```R
setup <- function(env) {
  env$time <- Sys.time()
  cat("Tests are running...\n")
}

teardown <- function(env) {
  cat("All tests ran in ", Sys.time() - env$time, " seconds.\n")
}
```

then we will get messages printed before and after we run all the tests. Notice
we can use the single parameter `env` of the function to pass information from
`setup` to `teardown` (if we want more organization, we can pass some hierarchically
nested list of functions to `setup` or `teardown`, in the background turned into
a [`stageRunner`](http://github.com/robertzk/stagerunner)).

In particular, we use this to ensure that all directories and all idempotent resources
[have README files](check_readme.R) like the one you are reading! If a resource
is not documented, tests will not run (and so the build for this repository will fail
on Travis, our continuous integration server) until a README is created.

*Note*: ***Idempotent resources*** are resources with helper files that are not themselves
resources, and whose name is the same as their parent directory. See the
[director](http://github.com/robertzk/director) package for more information.

Optional tests
-----

By default, this Syberia project enforces that all resources must have tests:
models, controllers, mungebits, etc., or we will not be able to tell long-term
if our code is correct as complexity grows.

Occasionally, some resources may be extraordinarily difficult to test (e.g., those
communicating with external sources), and it may be a reasonable sacrifice to exclude
them from testing.

Note that `test_project` will fail unless all resources (except the optional resource)
have tests.

To set the optional tests, simply write something like `optional_tests <- c('resource1', 'blah/resource2', ...)`
in your `test.R` file.

Pollution of the global or options namespace
-------

Note that if the setup, teardown hook, or any tests make changes to the global
environment or set of global options (see `?options` in R) without remembering to change them back,
Syberia by default will complain and refuse to run `test_project`. This is a bad practice
and you should make sure your tests do not pollute the global environment or the
global options space.
