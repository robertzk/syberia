Controllers
=========

*In another moment down went Alice after it, never once considering how in the world she was to get out again.  
The rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well.*
*- Lewis Carroll*

What is a controller?
-----------

In the same way that adapters are the core of reading and writing data and
classifiers are the core of training and predicting a model, controllers
allow us to define how to compile things like adapters and classifiers
from their individual files into the complete objects (e.g., something
of class `adapter` or `tundraContainer`, respectively,
with methods `read` / `write` and `train` / `predict`,
respectively).

A controller, at its most basic, is a function responsible for taking R scripts and
converting them to other things. For example,

```R
function(output) { output }
```

This is the most simple type of controller. It will process R scripts it is assigned
to "control" by converting them to their return value (the last expression evaluated
in the file). For example, if we had a file

```R
x <- 1
y <- x + 1
y + x
```

then the controller would give `3` when parsing this file. On the other hand,
controllers actually give us access to the environment that was used to execute the
file, so a controller like

```R
function(input) { list(x = input$x, y = input$y) }
```

would return `list(x = 1, y = 2)` (the variables `x` and `y` assigned during the
execution of the file are available later to the controller).

The full list of available options is:

  * `output` - The return value of the sourced file.
  * `input`  - An environment that contains all of the local variables defined in the file.
  * `resource` - The name of the sourced resource. For example, if we had a
    `models/dev/default/simple.R`, then `resource` would be `models/dev/default/simple`.
  * `modified` - Whether or not this file, or any of the things it depends on,
    has been detected as having been modified since it was last executed. This is useful if
    you are caching results in the processor and would like to re-use something from the
    cache when re-compiling it with the processor.
  * `resource_body` - The character representation of the file that was sourced.

Some of the more advanced options are:

  * `director` - The underlying [`director`](https://github.com/robertzk/director) object.
  * `preprocessor_output` - An environment provided to the preprocessor (read on to find 
    out more about this) that can be used to pass information to the controller
    after the file has been sourced.
  * `resource_object` - The underlying `directorResource` object with methods
    like `value` or `compile`.
  * `args` - The arguments to the `$value()` call on the `directorResource` object 
    to pass additional arguments to the processor.

In practice, the most important will be `input`, `output`, `resource`, and `modified`.

Preprocessors
------------

Before the R file actually gets sourced, we may want to perform some additional operations
before sourcing. For example, if we have

```R
y <- x + 1
y ^ 2
```

we can provide the value of `x` without requiring a global parameter by using
a preprocessor:

```R
preprocessor <- function(source_args, source) {
  source_args$local$x <- 1
  source()
} # The preprocessor

function(output) { output } # The processor
```

In this case, we are using the `source_args` built-in that gets passed to the `preprocessor`
(which we can define by putting a `preprocessor` local function in our controller) to
change the local environment provided when sourcing the file. (The built-in 
`source` is just a `do.call(base::source, source_args)`.)

While preprocessors do not have `input` or `output` (which wouldn't make sense, since
we have not sourced our file yet), they do receive

  * `source_args` - A list of options to pass to `base::source`. By default, this 
    will be a list with two elements, the absolute file name and an environment
    with additional local variables to populate before executing the file.
  * `source` - A shortcut for `do.call(base::source, source_args)`.
  * `preprocessor_output` - An environment provided to the preprocessor that can
    be used to pass information to the controller.

When used together, preprocessors and controllers are powerful tools that can
be used to greatly simplify the amount of work necessary to define adapters,
mungebits, classifiers, and even models themselves. (Everything in this syberia
project is just a resource controlled by a controller.)

Routes
-------

How do we actually ensure that a given controller gets executed on a particular
file? The answer is the [`config/routes`](../../config#routesr) file.

Just like with [Rails routes](http://guides.rubyonrails.org/routing.html), the routes
file allows us to specify which files are associated with which controllers. If
we have

```R
list('lib/adapters' = 'adapters', 'lib/mungebits' = 'mungebits')
```

this would tell the syberia project that files in `lib/adapters` should be processed
by the `adapters` controller (in `lib/controllers/adapters.R`) and files in
`lib/mungebits` should be processed by the `mungebits` controller
(in `lib/controllers/mungebits.R`).


