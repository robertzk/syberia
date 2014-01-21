.onAttach <- function(...) {
  load_github_packages(.github_packages)
  if (!exists('run')) makeActiveBinding('run', function() build_model, .GlobalEnv)
}

