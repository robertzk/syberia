Testing mungebits
=================

When writing tests for mungebits, we have to very careful to test
all the possible inputs if the mungebit is parametrized. You should
strive to completely test all the possible combinations of parameters
to your mungebit.

Train versus predict
--------------------

As usual, the main advantage of using mungebits for data preparation
is their implicit emphasis on the differences between
[training and prediction](../../../lib/mungebits): the same mathematical
operation on a data set may not be the same during training as it is
for prediction, and require different code.

An excellent mungebit test will ensure that the same operation applied
during training is also applied during prediction. You can do this
by calling `$run()` on the mungebit twice with the same input data set,
and comparing the outputs.
