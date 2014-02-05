.onAttach <- function(...) {
  #load_github_packages(.github_packages)
  if (exists('run', envir = .GlobalEnv, inherits = FALSE)) rm('run', envir = .GlobalEnv)
  makeActiveBinding('run', function() build_model, .GlobalEnv)
}

