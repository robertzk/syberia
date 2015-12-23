# The default IO adapter to use. If your syberia project's
# \code{"config/application.R"} has a \code{default_adapter} defined,
# it will be that; otherwise, it will be the file adapter.
local({
  default_value <- NULL
  `%||%` <- function(x, y) if (is.null(x)) y else x
  function() {
    default_value <<- default_value %||%
      resource('config/application')$default_adapter %||% 'file'
  }
})()
