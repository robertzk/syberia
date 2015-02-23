.onAttach <- function(...) {
  if (!isTRUE(getOption("syberia.silent"))) {
    packageStartupMessage(paste0("Loading ", director:::colourise('Syberia', 'red'), "...\n"))
  }
  print(getwd())
  # TODO: (RK) Find syberia project off getwd() and call syberia_project().
}

