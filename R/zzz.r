.onAttach <- function(...) {
  if (!isTRUE(getOption("syberia.silent"))) {
    packageStartupMessage(paste0("Loading ", director:::colourise('Syberia', 'red'), "...\n"))
  }
}

