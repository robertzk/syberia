## A dictionary of messages used by the package.
## We separate these into its own file to avoid cluttering
## the R code with a multitude of strings.
messages <- list(
  test_engine_type_error = c(
    "Please pass a ", sQuote(crayon::yellow("syberia_engine")), " object ",
    "to the ", sQuote(crayon::yellow("test_engine")), " function."
  ),
  
  test_hook_no_engine = c(
    "To fetch the {{{type}}} hook for a project, please pass in a syberia_engine ",
    "object (the syberia_engine for the syberia project). Instead I got ",
    "an object of class {{{klass}}}."
  ),

  test_hook_invalid_format = c(
    "Test {{{type}}} hooks must be a function or a list of functions.\n\nIn ",
    "{{{filename}}}, ensure that ",
    "you have ", sQuote(crayon::yellow("{{{type}}} <- some_function")),
    " as right now it's an object of class ",
    sQuote(crayon::red("{{{klass}}}"))
  ),

  test_hook_arity_error = c(
    "Test {{{type}}} hooks must all be functions that take at least one ",
    "argument.\n\nThe first argument will be an environment that has one ",
    "key, ", sQuote("project"), ". In {{{filename}}}",
    " ensure your ", sQuote(crayon::yellow("{{{type}}}")),
    " local variable meets this constraint."
  )
)

## Cleanse the message a little after fetching it from the `messages` list.
msg <- function(name) {
  stopifnot(name %in% names(messages))

  ## The `gsub` will squish multiple spaces into a single space,
  ## while the `paste(collapse = "", ...)` usage will ensure we
  ## can take vectors of characters in the above `messages` list.
  paste(collapse = "", gsub("[ ]+", " ", messages[[name]]))
}

## We use the [whisker](https://github.com/edwindj/whisker) templating
## engine to inject any additional values into the message string.
## For example,
## 
## ```r
## m("parse_mungepiece_dual_error", error = "Bloop")
## ```
##
## would return the appropriate error with the string "Bloop" injected
## in the appropriate place.
m <- function(name, ...) {
  ## Note the use of [`do.call`](http://www.inside-r.org/r-doc/base/do.call),
  ## a very handy R metaprogramming tool when we do not know exactly which 
  ## arguments we will pass.
  do.call(whisker::whisker.render, list(msg(name), list(...)))
}

