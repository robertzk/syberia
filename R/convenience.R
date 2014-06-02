# A list of convenient helper functions for interacting with syberia
# projects through the console.

#' Fetch active stagerunner
#'
#' @export
active_runner <- function() {
  syberiaStructure:::get_cache('last_stagerunner')
}

# Wrap S3 class for type detection
trigger <- function(fn) {
  class(fn) <- c('syberiaTrigger', 'trigger', class(fn))
  fn
}

#' Record dataframe in a global variable for debugging
#' @param varname a character vector. The name of the global variable
#'    to assign the dataframe to.
#' @param envir an environment. The place where to put the dataframe.
#'    By default, this is \code{globalenv()}.
#' @export
record <- function(varname, envir = globalenv()) {
  trigger(function(dataframe) {
    assign(varname, dataframe, envir = envir)
  })
}

#' @export
is.trigger <- function(x) inherits(x, 'trigger')

#' The before and after environments of the last syberia run.
#'
#' @return a list with \code{before} and \code{after} keys giving
#'    what the modeling environment looked like before and after
#'    the last syberia run.
#' @export
last_run <- function() syberiaStructure:::get_cache('last_run')

