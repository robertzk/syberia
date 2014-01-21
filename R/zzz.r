.onAttach <- function(...) {
  load_github_packages(.github_packages)
  if (exists('run')) rm('run')
  makeActiveBinding('run', function() build_model, .GlobalEnv)
}

