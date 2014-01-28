## This file compiles all preprocessing triggers

# Wrap S3 class for type detection
trigger <- function(fn) {
  class(fn) <- c('syberiaTrigger', 'trigger', class(fn))
  fn
}

#' Record dataframe in a global variable for debugging
#' @export
record <- function(varname, envir = globalenv()) {
  trigger(function(dataframe) {
    assign(varname, dataframe, envir = envir)
  })
}


is.trigger <- function(x) inherits(x, 'trigger')
