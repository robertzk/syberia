# If a test should be treated as a plain resource with access to
# the usual helpers ("resource", "helper", "resource_exists"),
# for example, when it needs to pull in another resource,
# it can use this controller.
preprocessor <- function(source) { library(testthat); source() }

function() { }
