.onAttach <- function(...) {
  if (!identical(silent <- getOption("syberia.silent"), TRUE))
    message(paste0("Loading ", crayon::red('Syberia'), "...\n"))

  load_github_packages(.github_packages, silent = silent)

  if (exists('run', envir = .GlobalEnv, inherits = FALSE))
    rm('run', envir = .GlobalEnv)

  makeActiveBinding('run', function() run_model, .GlobalEnv)

  addTaskCallback(auto_run)
}

