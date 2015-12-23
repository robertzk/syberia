Non-model helper resources
=========

Whereas [`models`](../models) contains all of the active models, the `lib` directory
is responsible for ensuring you [don't repeat yourself](http://en.wikipedia.org/wiki/Don't_repeat_yourself).

With projects that are expected to grow increasingly complex, it is imperative to separate
out code that is used in two or more places for re-use. Otherwise, any modification to
one instance of the code needs to be accompanied with a modification of all the identical
copies. Additionally, pulling out code makes it much easier to test and improve down the
road.

Any code or objects that could be
useful to multiple models or perform some functionally separable activity should reside
somewhere in `lib`. Some of the kinds of objects defined in `lib` are custom [`classifiers`](classifiers),
[`stages`](stages) (different steps in the modeling process, like importing or data preprocessing),
[`adapters`](adapters) (objects with `$read` and `$write` methods for reading and storing data and/or
models), [`controllers`](controllers) (the heart of Syberia's configurability), [`shared`](shared) (for re-usable
miscellaneous components) and [`mungebits`](mungebits) (for custom data preprocessing steps).

For a more detailed explanation of each type of resource, see that specific directory's
README file.
