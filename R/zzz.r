.onAttach <- function(...) {
  if (!identical(silent <- getOption("syberia.silent"), TRUE))
    message(paste0("Loading ", crayon::red('Syberia'), "...\n"))

  load_github_packages(.github_packages, silent = silent)

  if (exists('run', envir = .GlobalEnv, inherits = FALSE))
    rm('run', envir = .GlobalEnv)

  makeActiveBinding('run', function() run_model, .GlobalEnv)

  addTaskCallback(auto_run)

  tryCatch(syberia_project(), error = function(e) {
    if (grepl("invalid 'path'", conditionMessage(e), fixed = TRUE)) {
      message("...Call `syberia_project()` from within a Syberia directory to get started.")
    } else {
      stop(e)
    }
  })
}
