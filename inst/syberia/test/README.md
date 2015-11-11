# Writing tests for Syberia projects

Recall that in a Syberia project almost everything is a [helper or resource](../lib).
This allows for easy re-use of individual components. For example, you can include
a `mungebit` anywhere in this project by writing `resource('lib/mungebits/some_mungebit')`,
and it will be correctly converted into a `mungebit` object using the
[`mungebits controller`](../lib/controllers/mungebits.R).

This philosophy extends further: to make sure that each resource works as we expect,
we can write [*tests*](http://adv-r.had.co.nz/Tests.html) that verify every expected
input yields the appropriate output. Writing tests can be cumbersome at first, but
the long-term advantage is that we don't have to worry about breaking other developers'
code: if we make a change in one part of the system that breaks assumptions elsewhere,
it will be caught by our resulting failing tests.

Every time you write a resource, you should create its accompanying tests and put
some thought into what inputs are possible. You can use functions from the
[testthat package](http://github.com/hadley/testthat) or the
[testthatsomemore package](http://github.com/robertzk/testthatsomemore) to verify
everything is working as expected.

Cheat sheet
-----------

*If this is your first time reading about tests, skip to the next section.*

Assuming your Syberia project resides in `~/dev/awesome-project`, you
can write `test_project("~/dev/awesome-project")`, or simply `test_project()` if you
have already called `syberia_project("~/dev/awesome-project")` at some point.

To test a single resource at a time (instead of the entire project), you
can use the `stest` helper (defined in the [globals](../config/global.R)):
for example, `stest("lib/stage/import")` to test the [import stage](../lib/stages/import.R).

From within each test, you can use `resource()` to build the resource attached to each test.

A simple example of a test
--------------------------

Imagine we have a [`mungebit`](../lib/mungebits) that takes two variables and
creates a new variable that consists of their difference. This could look like:

```r
# lib/mungebits/differ.R
train <- function(dataframe, variable1, variable2, new_variable) {
  eval.parent(substitute({
    dataframe[[new_variable]] <- dataframe[[variable1]] - dataframe[[variable2]]
  }))
}

predict <- train
```

We can write tests for this mungebit by placing a file in `test/lib/mungebits/differ.R`.
In general, if there is a resource in location `X`, you can write a test for that
resource in `test/X`. Note this is true even if it is an idempotent resource
(i.e., a resource whose directory name is the same as its `.R` file). If we
had a complicated mungebit in `lib/mungebits/differ/differ.R`, its test
would still belong in `test/lib/mungebits/differ.R`.

Here's an example of what a test file might look like.

```r
# test/lib/mungebits/differ.R

test_that("it can subtract two variables in a dataframe correctly", {
  mp <- mungebits:::mungeplane(iris)
  mb <- resource() # This will be explained below.
  mb$run(mp, "Sepal.Length", "Petal.Length", "Sepal-Petal.Diff")
  expect_equal(iris[[1]] - iris[[3]], mp$data[['Sepal-Petal.Diff']],
    info = "there should be a new variable Sepal-Petal.Diff")
})
```

Inside a test, there is a special keyword `resource` available. Calling
`resource()` creates an instance of the Syberia resource being tested
(in this case, the `differ` mungebit). Since this creates a new resource each
time, this allows our tests to begin afresh every time we are in a `test_that` block.

How are tests implemented?
--------------------------

In general, tests are treated by Syberia as *just another resource*. This means
we can write our own [controllers](../lib/controllers) if we need to customize what
is available to our tests. For example, when testing [models](../models), we
do not want to use `test_that` blocks. A model resource is simply the stageRunner
for the model, and we would not want to run the entirety of the
[import stage](../lib/stages/import.R) when testing a model, so there is no
immediate way to test the stageRunner.

Instead, we can write a [separate controller](../lib/controllers/test)
for how model tests should be interpreted, so that any files ending up in
`test/models` behave differently to normal tests. In the case of the
[model tests controller](../lib/controllers/test/models.R), the compromise chosen
was to select 100 random rows from the full training set, and only run
[data stage](../lib/stages/data.R) during tests.
