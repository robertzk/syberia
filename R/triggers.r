## This file compiles all preprocessing triggers

# Wrap S3 class for type detection
trigger <- function(fn) {
  class(fn) <- c('syberiaTrigger', class(fn))
  fn
}

#' Record dataframe in a global variable for debugging
#' @export
record <- function(varname, envir = globalenv()) {
  list(function(dataframe) {
    assign(varname, dataframe, envir = envir)
  })
}
record <- trigger(record)


is.trigger <- function(x) inherits(x, 'syberiaTrigger')
