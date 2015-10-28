preprocessor <- function(source_env, source) {
  source_env$first  <- 1
  source_env$second <- 2
  source()
}

function(output) {
  output
}

