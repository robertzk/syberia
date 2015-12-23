Classifiers
========

In the same way that [mungebits](../mungebits) are the heart of data preparation,
classifiers are the actual machine learning core. In short, a classifier is
an object that has two methods, `$train` and `$predict` which are designed
to make training and prediction as simple as possible: both methods should
take only a `data.frame`.

A classifier is responsible for recording the distinction between training
versus prediction with a classifier (like a logistic regression or a decision tree model), 
by constructing a [`tundraContainer`](http://github.com/robertzk/tundra). This latter
object is a combination of data preparation and actual prediction. In short, a
`tundraContainer` object should be able to take a `data.frame`, whether one row
of data coming from production during live scoring, or a large test set that
needs batch prediction, and completely carry out the process of executing
the munging procedure (i.e., perform all necessary data preparation),
and finally turning the resulting data into a score (or scores for more
complicated outputs).

Consider the following simple example.

```r
train <- function(dataframe) {
  na.action <- input$na.action %||% "na.omit"
  output <<- list(model = lm(input$formula, dataframe, na.action = na.action))
}

predict <- function(dataframe) {
  type <- input$type %||% 'response'
  predict(output$model, newdata = dataframe, type = type)
}
```

First, notice we only define two functions, `train` and `predict`, both taking
a single argument. The `input` and `output` locals are special variables that
are made available to the `train` and `predict` methods and are fixed by
some external source. Essentially, this is saying "I would like to fix the
following parameters for this model -- now how do I train using this data set,
and then generate scores for a different data set, without having to think more
or make any more choices?" By taking this approach, it becomes incredibly simple
to use a "model object" in production: simply call its `$predict` method on the
`data.frame` you wish to score.

**Note**: The special in-fix operator `%||%` is provided by Syberia and
means "if the first value is `NULL`, use the latter, otherwise use the former",
and is almost equivalent to Ruby's `||` operator.

The above code could be translated into words as: "to train this model, take
a fixed parameter `na.action`, and if none is given, assume 'na.omit'. Then, fit
a linear regression" and "to predict using this trained model, take a fixed
parameter `type`, and if none is given, assume 'response', then use the
standard `predict` method on this `lm` object to get the predicted linear
extrapolation."

Let's see what happens when we ask for this resource by placing the above
in a file `lm.R` and calling from the console `resource('lib/classifiers/lm')`
(after making sure our current directory is indeed somewhere in this repository).
We observe

```r
function(munge_procedure = list(), default_args = list(), internal = list()) {
  tundra:::tundra_container$new(resource, input$train, input$predict,
                                munge_procedure, default_args, internal)
}
```

What has happened under the hood is that the train and predict functions were
parsed out and turned into a function: a `tundraContainer` creation function.
By giving three parameters, a `munge_procedure` (just a `list` of [`mungebit`s](../mungebits)),
`default_args` (for example, `list(na.action = 'na.fail', type = 'terms')`),
and `internal` (any extra data we want to associate with our classifier),
we will have a fresh `tundraContainer` that is ready to train given some
data set (using `$train(data_set)`). Once trained, we will only be
able to `$predict` using this object.

Once we have built a `tundraContainer` by calling the above function, we can
use it to train models (note that the second argument to `train` below comes
from a [different place](https://github.com/robertzk/tundra/blob/master/R/tundra_container.r#L40)
than the `train` function above, which was a template that will get passed the
second argument through the `input` variable).

```R
lm_maker <- resource('lib/classifiers/lm')
lm_model <- lm_maker(list(), list(na.action = 'na.fail')) # No data preparation for now, just give us a tundraContainer
lm_model$train(iris, list(formula = Sepal.Length ~ Sepal.Width))
head(lm_model$predict(iris))
#        1        2        3        4        5        6
# 5.744459 5.856139 5.811467 5.833803 5.722123 5.655114
```

While this may seem silly for this simple example (since R was designed to make
things like linear regressions simple to write), it becomes very powerful
for more complicated methods that require a lot of parameters with complicated
data preparation procedures.
