Testing stages
==========

You should make sure that all of the standard parameters for your stage 
are being handled gracefully. For example, when testing import stage,
we should check that the `R`, `s3`, and `file` parameters are
loading data like we expect. (Although perhaps s3 would be difficult
given the connectivity issues. You need to make use of
[stubbing](http://github.com/robertzk/testthatsomemore) in your tests
to ensure that your tests are self contained and do not reference
external resources.)

