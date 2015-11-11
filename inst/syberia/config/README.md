Configuration Files
========

As with Rails, the `config` directory is intended for files that are
used for configuring the project or otherwise providing global settings.
At the moment, this includes the following.

[application.R](application.R)
------------

Like with Rails's `application.rb` file,
this provides configuration options that should be global to all environments
(test, development, and production). Imagine you have

```R
a <- 1
b <- 'some_value'
c <- list(x = 1)
```

in your `application.R` file. Then calling

```R
syberia_project()$resource('config/application')$value()
```

will yield the list `list(a = 1, b = 'some_value', c = list(x = 1)`. Usually,
this is used in resources like adapters or models using
`resource('config/application')$a` (note that from within a resource rather than
the command line, we do not need to write `$value()`).

[routes.R](routes.R)
--------

This file should return a list that contains a
collection of routes, analogous to Rails routes. In order to explain what these
are, we need to give some background.

Every Syberia project can ask for its underlying [`director`](http://github.com/robertzk/director)
object using `syberia_project('path/to/project')` (in my case, it is `~/dev/analytics`).
Say we assign this to a variable

```R
d <- syberia_project('path/to/project')
```

Now we can load almost any R file in the repository using, for example,

```R
d$resource('config/routes')$value()
```

where we need to call `$value()` in order to "compile" the resource. This means that
instead of just calling `base::source` on it to execute the script, we may *do more*
with the file. This is covered in the section on [controllers](../lib/controllers).

A controller is just a resource (i.e, some R code) that tells us how to translate
our source file into something more interesting (a [mungebit](../lib/mungebits),
[stagerunner](../lib/stages), etc.). How it does so is not important to understanding
routes: the point is that if your `routes.R` file looks like the one below,

```R
list(
  'lib/adapters'    = 'adapters',
  'lib/classifiers' = 'classifiers'
)
```

then this is telling Syberia that every possible resource under `lib/adapters`
will use the `adapters` controller (located in `lib/controllers/adapters`) and
every possible resource under `lib/classifiers` will use the `classifiers` controller
(located in `lib/controllers/classifiers`).

For example, if we would like to grab the glmnet classifier, we can do so from the
R command line with

```R
d$resource('lib/classifiers/glmnet')$value()
```

or we can do so from another resource (say, a model), with

```
resource('lib/classifiers/glmnet')
```

and the result will be a fresh [`tundraContainer`](../lib/classifiers).

In general, `routes.R` should look like

```R
list(
  path_prefix1 = 'controller1',
  path_prefix2 = 'controller2',
  ...
)
```

and Syberia will set up the logic so that calling `d$resource('path_prefix1/...')`
executes the appropriate `controller1`.

If this discussion is still unclear, consider reviewing the
[Rails routes guide](http://guides.rubyonrails.org/routing.html)
for an understanding of where the inspiration for Syberia routes originated.

[environments](environments)
--------

Files in this directory have the same purpose as the `application.R` file, with
the exception that they are sourced only under certain circumstances.

For the moment being, this distinction is only made for the `test` environment.
When executing [`test_project`](https://github.com/robertzk/syberia/blob/master/R/tests.R),
it may appeal to `config/environments/test` for some of its configuration values.

[initializers](initializers)
---------

As with [Rails initializers](http://guides.rubyonrails.org/configuring.html), any code
that is configuration and/or startup related for packages or plugins not related to the
core Syberia project should be placed in `config/initializers`. For example, if you
are using `knitr` or `Rmarkdown` with some custom settings that you would like to
not pollute the global options space with, you can place them in `config/initializers/knitr`
or `config/initializers/Rmarkdown` and access those resources with
`resource('config/initializers/knitr)` or `resource('config/initializers/Rmarkdown)` within
other Syberia resources (models or `lib` objects).
