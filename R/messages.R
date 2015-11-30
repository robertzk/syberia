## A dictionary of messages used by the package.
## We separate these into its own file to avoid cluttering
## the R code with a multitude of strings.
messages <- list(
  test_engine_type_error = c(
    "Please pass a ", sQuote(crayon::yellow("syberia_engine")), " object ",
    "to the ", sQuote(crayon::yellow("test_engine")), " function."
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


