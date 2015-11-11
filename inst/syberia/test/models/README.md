# Testing models

Every resource in a Syberia project should have accompanying tests. This includes
models.

A model is just a stageRunner constructed from [its model file](../../models), 
so how can we "test" a model? In particular, if we have a model that is importing data,
we do not want to reproduce that import when testing, since it would take a long
time! (and we do not have the appropriate S3 or other credentials on the continuous
integration server)

The solution is to run `test_project()` locally whenever you build a new model: this
will execute the `import` stage one time and store 100 randomly chosen records
in the `test/.registry` directory (relative to the root of the project). When
the tests are run in continuous integration (i.e., Travis), the [data stage](../../lib/stages/data.R)
will be executed on those 100 rows.

*Note*: Running `test_project()` will only populate the 100 training rows for
new moels not in the test registry. If you wish to test an individual model,
you can use `stest("models/prod/default/en-US/2.2.1")` (replaced with your model version).
Sometimes, an error will occur in continuous integration that you can't reproduce remotely.
In these cases, it may be helpful to pretend to be Travis: `options(TRAVIS = 'TRUE')`.
Running `stest` may now produce the same error (and running `test_project()` will
take a long time as it will test all the models).


