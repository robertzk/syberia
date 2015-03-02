.onAttach <- function(...) {
  if (!isTRUE(getOption("syberia.silent"))) {
    packageStartupMessage(paste0("Loading ", director:::colourise('Syberia', 'red'), "...\n"))
  }
  
  # We want to initialize a Syberia project in the current working directory
  # because 9 times out of 10 this is what the user wants.
  #
  # However, this hook doesn't work on install, because install is done from
  # the working directory of the package, not the user.
  #
  # But when the user calls library(syberia), this will work.
  try(syberia_project(), silent = TRUE)
}
