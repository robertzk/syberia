.onAttach <- function(...) {
  load_github_packages(.github_packages)
  makeActiveBinding('run', function() build_model, .GlobalEnv)
}

