# Version 0.4.1

* Inside of syberia models, the local variables `model_version`, `model_name`,
  and `output` are now available. The former two are the full model version
  (e.g., `"default/en-US/1.0"`) and model name (`"1.0"`), respectively, whereas
  `output` is a helper function that outputs to the `tmp` directory relative
  to the root of the syberia project. For example, `output("blah.csv")` 
  yields `"<syberia_project_root>/tmp/default/en-US/1.0/blah.csv"`. This can
  be very helpful for temporary output files, like a copy of the model or
  presentation files (like plots). The behavior of `output` may change in the
  future, but for now it is fixed to the "tmp" directory.

  Finally, there is also a `root` helper for the root of the current syberia project.

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

