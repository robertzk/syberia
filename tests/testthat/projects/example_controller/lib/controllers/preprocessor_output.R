preprocessor <- function(preprocessor_output) {
  preprocessor_output$hello <- "world"
}

function(preprocessor_output) {
  as.list(preprocessor_output)
}
