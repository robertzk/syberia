.onAttach <- function(...) {
  if (!identical(silent <- getOption("syberia.silent"), TRUE)) {
    message(paste0("Loading ", director:::colourise('Syberia', 'red'), "...\n"))
  }

  if (exists('run', envir = .GlobalEnv, inherits = FALSE)) {
    rm('run', envir = .GlobalEnv)
  }

  makeActiveBinding('run', function() run_model, .GlobalEnv)

  if (!isTRUE(getOption('syberia.do_not_autorun'))) {
    addTaskCallback(auto_run)
  }
}

