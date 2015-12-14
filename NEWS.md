# Version 0.4.4.3

* Allow `resource` to be called in tests to fetch other resources.

# Version 0.4.4.1

* Small changes to bring compatibility with stagerunner 0.5.0

# Version 0.4.2

* Users can provide custom bootstrapping (i.e., things to do the first
  time the syberia project is loaded) by defining a `config/boot` resource.
  For example, placing `cat("Bootstrapped")` in `config/boot.R` will
  print "Bootstrapped" every time the syberia project is loaded for the
  first time in a fresh console.

# Version 0.4.1

* This version was released in conjunction with version 0.2.0 of
  syberiaStages and version 0.2.0 of syberiaStructure. The addition
  is the notion of Syberia IO "adapters".

  IO adapters are (reference class) objects that have a \code{read}
  and \code{write} method. By wrapping things in an adapter, you do not have to
  worry about whether to use, e.g., \code{read.csv} versus \code{s3read}
  or \code{write.csv} versus \code{s3store}. If you are familiar with
  the tundra package, think of adapters as like tundra containers for
  importing and exporting data.

# Version 0.4.0

* `syberia_version` function for printing the current syberia version.

* In `data_stage`, the mungebits are now called with `train_only`, which
  prevents them from getting trained so that the predict function does
  not accidentally run when re-running with the `run` helper method.

* Customer tundra containers (i.e., classifiers) can now be stored in the
  `lib/classifiers` directory of your syberia project. If you do this,
  you need to use the same keyword as the first argument to the `model_stage`
  as the filename within the `lib/classifiers` directory, and include a
  function named `train` and `predict`.

* When running a model, all of its helper files are also tracked for
  modifications, so that the cached environment for each step can be loaded.
  If this happens, Syberia will print "Copying cached environments..." when
  running the model (to prevent confusion when it doesn't happen -- for example,
  if you forgot to save one of the helpers).

* Support for unit testing on individual model files. The unit tests must
  go in `models/test` under the same file path as your model version. So
  if you have a model in `models/dev/models_one/model1.r`, you must put
  unit tests in `models/test/models_one/model1.r`. This file must have a
  list structure similar to the one in the model, except the terminal nodes
  will be functions of the form `function(before, after) { ... }`, where
  `before` and `after` are environments holding the modeling environment
  before and after executing that step.

* Better support for printing what models are running. The absolute file
  path will now be listed.
