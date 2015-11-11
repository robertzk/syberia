The models controller
=====================

To run a model, we usually need to do several things:

   * Import our data
   * Run data preparation
   * Run the actual model
   * Export the model
   * (Optional) Validate our results

To make this process expedient, the modeling process has been wrapped around a [stagerunner](http://github.com/robertzk/stagerunner)
that is responsible for executing each step in the modeling process. The advantage of using a stagerunner instead of a collection
of scripts is that everything is self-contained in one object, and the stagerunner caches our progress. This latter part is critical:
let's say we are playing around with a data preparation step that we haven't gotten quite right. By using a stagerunner, we
can re-execute just that data prep step multiple times without having to run the process from scratch or digging around in our
long file to execute the right bits of code. It becomes as simple as `run('data/our step')` and the underlying stagerunner will
do all the work.

Everything in this syberia project is [just a resource](..). Even a model stagerunner is constructed from scratch using nothing
except the controller and preprocessor defined in this directory. Let's go through some of the things that need to happen to
construct a model stagerunner:

  * Mungebits need to be injected into the environment. For example, if we have `lib/mungebits/some_mungebit`, we would like
    the local variable `some_mungebit` to be available to our model file without having to explicitly fetch it with
    `resource('lib/mungebits/some_mungebit')`.
  * Special shortcuts could be defined. For example, in the data stage, the convention
    `"Some munge step" = list(functionA ~ functionB, ...)` means "use `functionA` for training and `functionB` for prediction."
    The logic to inject this overloading of the `~` operator lives in `preprocessor.R`.
  * Some special values available to the model file: `model_version`, `model_name`, and `output` provide the model version
    (e..g. "default/en-US/2.1.3"), model name (e.g., "2.1.3"), and a generic function that can be used to create a temporary
    directory in which to place model output ("tmp/default/en-US/2.1.3" under the root of the project).

All of these are preprocessor steps. After the preprocessor has provided the model file a suitable environment, it
must read in the model file; something like

```R
list(
  import = 'some_file',

  data = list(
    'Drop unneeded variables' = ...,
    'Impute variables' = ...,
  ),

  model = list('regularization', alpha = 0.5),

  export = 'some_other_file.rds'
)
```

Once the model has been read in, we must construct the actual stagerunner. This happens in `construct_stage_runner.R` (in this directory).
However, before we do this, we have a special trick. If the model file has not changed at all since the last run, and none of its
dependencies (any resources it loaded, like [classifiers](../../classifiers) or [mungebits](../../mungebits)) have changed,
we should re-use the previous stagerunner.

On the other hand, if there are changes to the model file but we have already executed some of the stages (i.e., imported our data),
we don't want to lose our progress! The answer is to build a new stagerunner using the updated model file, and *coalesce* it onto the
cached stagerunner from the previous run. This means: given two stagerunners (essentially nested tree structures), copy all of the
cached environments that exist from one to the other. That is, on every step, the stagerunner is keeping track of what changes have
been made so far. We need to copy these changes over to the *new* updated stagerunner so that we can do things like re-execute the
3rd data preparation step without having to re-run import stage and the first 2 data preparation steps.

That logic is covered by the `models.R` file processor, around

```R
if (identical(resource, director$.cache$last_model) &&
    resource_object$any_dependencies_modified()) {
  message(testthat:::colourise("Copying cached environments...", "yellow"))
  stagerunner <- construct_stage_runner(output)
  stagerunner$coalesce(director$.cache$last_model_runner)
} else if (!is.element('last_model_runner', names(director$.cache))) {
  stagerunner <- construct_stage_runner(output)
} else {
  stagerunner <- director$.cache$last_model_runner
}
```

After this, we would like to wrap our unit tests for each stage. (TODO: (RK) Explain unit tests for stagerunners!) This is done
by *overlaying* a test stagerunner over the model stagerunner, so that at every terminal node (stage), the tests "wrap around"
the real action so they can examine the before and after environment.

Constructing a stagerunner
==============

To construct the model stagerunner, we loop over each of its stages (`import`, `data`, ...) and look in `lib/stages/import`,
`lib/stages/data`, etc. to grab the appropriate stage builder. A [stage builder](../../stages) is merely a function that takes the options
given in the model file (in the above example, the `import` stage would receive one argument, `'some_file'`) and
returns a function, a list of functions, or a stagerunner, to be embedded in the model stagerunner.

You should browse through the code in `construct_stage_runner.R` to understand how this is done.
