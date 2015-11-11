Models
=========

Any analytical model resides in the `models` directory. To view the list of available models, you can use

```R
syberia_models() # Display all model files
syberia_models('pdeu1') # Use fuzzy matching to find models
```

The second example requires a bit of explanation. Just like the [ctrl-p plugin for Vim](https://github.com/kien/ctrlp.vim),
`syberia_models` provides fuzzy matching to find models faster. The above
gets converted to the regular expression `".*p.*d.*e.*u.*1.*"`. That is,
any model file containing the consecutive characters "pdeu1" *somewhere* will
be returned (e.g. "**p** ro **d** /d **e** fa **u** lt/en_US/ **1** .0").

By default, the results of `syberia_models` are sorted in descending order
by last modified time, so the latest modified model satisfying the given filters
appears first. For more details, see `?syberia_models`.

Running models
==========

To run a model, just use:

```R
run('pdeu1')
```

using the same fuzzy matching as described above. Under the hood, this is using the
first result from `syberia_models("pdeu1")`. While "running" a model, you are
really running the [underlying stagerunner](https://github.com/robertzk/stagerunner).
This is an object that is recording the list of steps that have been executed so
far and that allows you to *replay* some steps. For example, image your model looks like

```R
# models/dev/example_model
list(
  import = "some_file.csv",
  data = list(
    "Filter some columns"   = list(drop_variables, "bad_column_name"),
    "Impute another column" = list(imputer, "credit_limit"),
    ...
  ),
  ...
)
```

and we execute it using `run('exmo')` ("dev/**ex**ample_**mo**del). If the model
errors on the imputation step, the progress of how we have gotten there is still
stored. We can make a change to step #2 (for example, if we realize the variable
is called something other than `"credit_limit"`), and re-execute it using:

```R
run(, 'data/impute')
run(, '2/2') # Another way to do it.
```

Note we can leave the first argument blank, since we already are executing some
model. By default, if you are running the steps to build a model, Syberia
remembers this and you can leave the first argument to `run` blank.

You can pass a second argument (`to`) to indicate a range of steps you
would like to execute.

```R
run(, 'import', to = 'data/Other munging step')
run(, 1, '2/13') # Another way to do it, assuming "Other munging step" is the 13rd data prep step
```

Organization of models
==============

Models are split between those in development and those in production. Any model
that is about to be deployed, is currently in production, or was once in production
should be placed in `prod`. **If there is any issue in this model, and the model is
already in production, it should not be modified.** Instead, a new model should be created.
The idea is that you should be able to replicate deterministically any model that
was constructed for production use.

Models that are still in development or are purely experimental should be placed
in `dev`. It is not necessary to have the same name for development as it is for
production.
