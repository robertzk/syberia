preprocessor <- function(filename) {
  list(prefilename = filename)
}

function(resource, filename, output) {
  list(resource_name = resource, filename = filename, prefilename = output$prefilename)
}

