Your new Syberia project
===============
[Syberia](http://github.com/robertzk/syberia) is a collection
of R packages that try to enforce [convention over configuration](http://en.wikipedia.org/wiki/Convention_over_configuration)
and [don't repeat yourself](http://en.wikipedia.org/wiki/Don't_repeat_yourself).

R codebases are typically loosely organized collections of scripts. By enforcing a structure that
encourages separating out components for re-use and enabling automated testing,
several long-term effects on the modeling process should emerge: research should be
reproducible, there should be no difference between experimenting with a new method
and developing something for production (i.e., development = production), and
complex interdependencies should be incapable of causing breakdowns as a result of
the inability of the developers to maintain such complexity.

Prerequisites
=========

While it should be possible to jump into some basic modeling straight away, it is important
to try to keep in mind that everything is an offspring of the following tools (all of them
based off [object-oriented programming](http://adv-r.had.co.nz/OO-essentials.html)):

  * **[Stagerunner](http://github.com/robertzk/stagerunner)** - The core object responsible
    for running models. The native workflow for a typical R programmer when processing data
    or playing with parameters is to re-execute files or pieces of files. While functional,
    this approach has a few drawbacks. The process of re-executing parts manually encourages
    code pollution through debugging / print statements and impacts long-term maintainability
    without a good habit of reverting these changes.

    It is difficult to know what parts to execute to achieve a specific outcome without
    reading the code in detail: if I know a model file imputes several variables, and I am
    debugging an issue I believe is related to this imputation, I have go to find which
    part is responsible first.

    It is difficult to organize the script in any canonical fashion other than through
    comment sections. Even if the correct organization is hierarchical, a file-based
    approach always encourages a flat linear structure.

    Working with `stageRunner` objects solves these issues. A `stageRunner` is merely a
    nested list of functions that each take one argument: [an environment](http://adv-r.had.co.nz/Environments.html)
    (you should be familiar with the R environment data structure). This environment
    is the "playground" for the functions, and as you pass through each one, you should
    be modifying this environment according to what you'd like to preserve across each
    step. For example, importing data should create a `data` variable in the environment,
    and modifying the data should modify this `data` variable in this environment.

    Behind the scenes, a `stageRunner` keeps track of every modification to the
    environment it is attached to (which we from now on refer to as its "context").
    You can "replay" these changes when debugging; if you are manipulating some data and reach
    the tenth step of data preparation and your data looks wrong, you can go back and
    look at what it was like in steps 1-9 without having to re-execute code from
    the beginning. For a more detailed example of how to do this,
    take a look at the [stageRunner interactive tutorial](http://en.wikipedia.org/wiki/Vaporware)
    (**TODO**: Make this.)

  * **[Mungebits](http://github.com/robertzk/mungebits)** - The core objects responsible for
    ensuring that the same data preparation occurs in training (development) and prediction
    (production).

    It is a tremendously under-appreciated fact that [data science is largely data janitorial
    work](http://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html).
    In other words, it is impossible to get significant insight without rolling up your
    sleeves and re-molding and manually fixing your data until it can be passed to a statistical
    algorithm. This is difficult enough as it is to do while developing a model.

    It is a far harder proposal to achieve the same consistency in data preparation during
    prediction. When launching a model in production so that it scores live customers,
    the data coming into the trained statistical algorithm should be qualitatively identical
    to the data that was used during training / development. That is, we must *replicate*
    the data preparation from training during prediction.

    Unfortunately, this is not as simple as re-executing the same code. For example, if we
    impute a column's missing values with its mean, we obviously cannot perform the
    same procedure on one data point; we must *remember* the mean, and use that cached
    information to perform a matching operation. This is a subtle but incredibly important
    point: in order to transform static, training data versus live, prediction data,
    it is possible that we must use completely different code to achieve the same mathematical
    transformation.

    A `mungebit` is an object with two methods, `train` and `predict`, with a special keyword
    available. In the `train` method, we can set things like `inputs$mean <<- mean(some_column)`
    in order to store (for example) a mean that we will need for live imputation. The `inputs`
    keyword is a variable that lives in a parent environment of the `train` method's
    environment, and can be modified using the `<<-` operator for use in the `predict`
    method.

    An abstract mungebit is usually independent of any data set: the idea of imputing a variable,
    dropping a column with many missing values, or performing [sure independence screening](http://onlinelibrary.wiley.com/store/10.1111/j.1467-9868.2008.00674.x/asset/j.1467-9868.2008.00674.x.pdf;jsessionid=978642E589014AA154A21BE2CE854D22.f01t01?v=1&t=i04x8nfw&s=8a5207bd8384e1ebe65fbd845f639d749b02cabc)
    are all operations that work on almost any data set. To record the dependence on some data
    set, we can wrap a `mungebit` in a `mungepiece`: an object that also has a `train` and
    `predict` method, but stores a `mungebit`, `train_args` (training arguments) and
    `predict_args` (predict arguments). For example, if we have a mungebit that aims to
    keep some set and only some set of fixed named variables, but we must be careful to
    drop the dependent variable during prediction, we can pass the variables we'd like to
    preserve separately for training and prediction. In this case, the mungepiece's `mungebit`
    would be a `mungebit` that generically preserves all but the given variables, its
    `train_args` would be our set of desired variables including the dependent, and `predict_args`
    would be this set excluding the dependent.

    Finally, one can use the [`munge` function](https://github.com/robertzk/mungebits/blob/master/R/munge.r) to execute a list of mungebits in succession
    on some `data.frame`. For a more detailed explanation, see the [interactive
    mungebits tutorial](http://en.wikipedia.org/wiki/Vaporware). (**TODO**: Make this.)

  * **[Tundra](http://github.com/robertzk/tundra)** - Training a model and having the correct
    settings during prediction can involve a lot of separate pieces of configuration.
    To solve this problem, a `tundraContainer` is an object that has two methods:
    `train` and `predict`, which take a data set, and run a "model" on that data
    set (for example, logistic regression or GBM). One can also think of a tundraContainer as
    a wrapper around both the native model object and the pre-processing methods used to generate the model

    However, this is only half of the story. When making predictions in a production
    environment, we have already pointed out that the data coming into the algorithm
    must look identical to the type of data the model was trained on. Therefore,
    we hereby define a *model* as being the union of both the actual mathematical
    algorithms that end up producing numerical outcomes **and** the data preparation
    procedure itself (which is highly customized to one specific data set).

    This sacrifices the generality of the classifier, since it must be fed very
    specific kind of data (namely, the kind of raw data the model was trained on
    before any preprocessing steps). However, it enables a more powerful procedure:
    given any raw unadulterated production data (whether historical / training, or
    live / production), we can instantly ask for its predicted values by passing
    the data to the `tundraContainer`'s `predict` method. There is no need to
    preprocess the data (this is done by the `tundraContainer`), or to give model
    prediction parameters (e.g., whether we're requesting probability or log odds).
    These have been fixed when training the classifier, as its sole purpose is to
    take raw data and produce a final score in a production environment without any
    further input.

    For more information on how to wrap your existing model scripts into `tundraContainers`,
    check out the [interactive tundra tutorial](http://en.wikipedia.org/wiki/Vaporware).
    (**TODO**: Make this.)

  * (*Optional*) **[Director](http://github.com/robertzk/director)** - Syberia itself
    is built on top of an object that contains all relevant information about the project:
    files, configurations, tests, etc. While it is not strictly necessary to understand
    the details of a director object to be productive with Syberia, it will help when
    writing new routes or controllers (see `lib/controllers` **TODO**: Link this).

Structure
========

While in theory, unlike most popular frameworks for structured development (e.g., Rails, Django, AngularJS),
Syberia is much looser about its conventions, and for the most part allows you to adopt
arbitrary directory structures, this generator enforces the following conventions.

  * **config** - This directory should be used for all configuration-related code. For example,
    `application.R` contains global configuration parameters, whereas `initializers`
    is intended to contain initialization scripts for add-on packages or plug-ins. Finally,
    `environments` is intended to be configuration for development versus testing versus production.

  * **lib** - This is the skeleton of the repository. Any code or objects that could be
    useful to multiple models or perform some functionally separable activity should reside
    somewhere in `lib`. Some of the kinds of objects defined in `lib` are custom `classifiers`,
    `stages` (different steps in the modeling process, like importing or data preprocessing),
    `adapters` (objects with `$read` and `$write` methods for reading and storing data and/or
    models), `controllers` (the heart of Syberia's configurability), `shared` (for re-usable
    miscellaneous components) and `mungebits` (for custom data preprocessing steps).

  * **models** - The heart of the project. All model files should be contained in this
    main directory. Models in `dev` are experimental, and models in `prod` have been
    deployed and are expected to remain static indefinitely.
