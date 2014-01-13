## This file compiles all preprocessing triggers

#' Record dataframe in a global variable for debugging
#' @export
record <- function(varname, envir = globalenv()) {
  list(function(dataframe) {
    assign(varname, dataframe, envir = envir)
  })
}
