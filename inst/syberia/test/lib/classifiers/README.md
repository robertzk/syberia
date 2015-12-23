Testing classifiers
==========

Since classifiers like glmnet are expensive to train, it
can be difficult to write tests for these resources.

For the moment, you can add your classifier to the list of optional tests
in [the test environment setup](../../../config/environments/test/test.R).

However, a better approach would be to use good stubbing and test that
the parameters to your classifier are getting formatted as expected
and throw errors when invalid values are passed.
