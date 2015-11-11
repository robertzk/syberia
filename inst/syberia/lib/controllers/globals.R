preprocessor <- function(source_env, director, source) {
  source_env$director <- director
  source()
}

function(input) {
  input
}
