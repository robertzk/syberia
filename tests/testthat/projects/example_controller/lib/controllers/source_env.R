preprocessor <- function(source_env, source) {
  source()
  as.list(source_env)
}

function(output) { output }
