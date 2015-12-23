Test controllers
==============

Whereas [regular controllers](..) are responsible for coordinating how to process resources like mungebits, classifiers, and models,
*test controllers* are responsible for defining how to unit test these resources. Usually, it will suffice to not define a controller
for a given resource. The default is the Syberia test controller, which will provide a function called `resource` that will construct
the resource we are testing. We can then write `test_that` statements to ensure our resource is behaving as expected (for example,
that our mungebit is processing example `data.frame`s as expected).

On the other hand, when testing models, we would like to run all the way up to the data preparation, but not further,
since we don't want to build a whole model for just a unit test! However, the data stage could take a long time. Instead, we
inject a mungebit that limits the data to the first 100 rows and make sure it runs smoothly, so that we can be confident
all of ours models are correctly processing their data.


Plain controller
-------------

TODO: (RK) Explain the plain controller.
